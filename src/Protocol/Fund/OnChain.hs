{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Fund.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger
import qualified Ledger.Ada                                      as LedgerAda
import qualified Ledger.Typed.Scripts                            as Scripts
import qualified Ledger.Value                                    as LedgerValue
import qualified Plutonomy
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsScripts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                       as LedgerContextsV2
import           PlutusTx                                        (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude
--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants                               as T
import qualified Generic.OnChainHelpers                          as OnChainHelpers
import qualified Generic.Types                                   as T
import qualified Protocol.Constants                              as T
import qualified Protocol.Fund.Helpers                           as Helpers
import qualified Protocol.Fund.Helpers                           as OnChainHelpers
import qualified Protocol.Fund.Holding.Types                     as FundHoldingT
import qualified Protocol.Fund.Types                             as T
import qualified Protocol.InvestUnit.Types                       as InvestUnitT
import qualified Protocol.Protocol.Types                         as ProtocolT
import qualified Protocol.Types                                  as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicy #-}
mkPolicy :: T.CS -> T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicy tokenMAYZ_CS T.PolicyParams {..} !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        &&
        validate then () else error ()
    where
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
        !protocolPolicyID_CS = ppProtocolPolicyID_CS
        !fundPolicy_TxOutRef = ppFundPolicy_TxOutRef
        !fundValidator_Hash = ppFundValidator_Hash
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- No hace falta, los dos parametros de la politica estan siendo utilizados en el codigo
    ------------------
        validate :: Bool
        validate = case redeemer of
                (T.PolicyRedeemerMintID _) -> validateMintAndBurnID
                (T.PolicyRedeemerBurnID _) -> validateMintAndBurnID
                (T.PolicyRedeemerMintFT _) -> validateMintAndBurnFT
                (T.PolicyRedeemerBurnFT _) -> validateMintAndBurnFT
    ------------------
        !fundPolicy_CS = LedgerContextsV2.ownCurrencySymbol ctx
    ------------------
        validateMintAndBurnID :: Bool
        validateMintAndBurnID =
            case redeemer of
                (T.PolicyRedeemerMintID _) ->
                    -- Que venga ProtocolDatum como ref
                    -- Para identificar el correcto ProtocolDatum necesita la póliza Protocol ID que está en los parámetros de esta póliza.
                    -- Que se genere salida con nuevo FundDatum en Fund Validator que esta en el datum
                    -- Que el FundDatum sea correcto según límites y valores del ProtocolDatum
                    -- Que se minteen todos los Fund IDs con own póliza
                    -- Que el FundDatum tenga el Fund ID
                    -- Que se generen, sean correctos, se minteen ID y que los tengan, los datums de InvestUnitDatum, OracleDatum y OracleReIDxDatum
                    -- Que se estén entregando los MAYZ correspondientes segun FundClass del FundDatum y según valores del ProtocolDatum para esa clase
                    ---------------------
                    traceIfFalse "not isTxOutAnInput" (OnChainHelpers.isTxOutAnInput fundPolicy_TxOutRef info)  &&
                    traceIfFalse "not isMintingIDs" isMintingIDs &&
                    traceIfFalse "not isCorrect_Outputs_Addresses" isCorrect_Outputs_Addresses &&
                    traceIfFalse "not isCorrect_Output_FundDatum" isCorrect_Output_FundDatum &&
                    traceIfFalse "not isCorrect_Output_FundDatum_Value" isCorrect_Output_FundDatum_Value &&
                    traceIfFalse "not isCorrect_Output_InvestUnitDatum_Value" isCorrect_Output_InvestUnitDatum_Value
                    ---------------------
                    where
                   ------------------
                        !inputsRef_TxOuts_And_ProtocolDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType ctx protocolID_AC ProtocolT.getProtocolDatumType
                        !inputRef_TxOut_And_ProtocolDatum = case inputsRef_TxOuts_And_ProtocolDatums of
                            [x] -> x
                            _   -> traceError "expected exactly one Protocol input ref"
                    ------------------
                        !outputs_TxOuts_And_FundDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_AC @T.ValidatorDatum @T.FundDatumType ctx fundID_AC T.getFundDatumType
                        !output_TxOut_And_FundDatum = case outputs_TxOuts_And_FundDatums of
                            [x] -> x
                            _   -> traceError "expected exactly one Fund output"
                    ------------------
                        !outputs_TxOuts_And_InvestUnitDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_AC @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType ctx investUnitID_AC InvestUnitT.getInvestUnitDatumType
                        !output_TxOut_And_InvestUnitDatum = case outputs_TxOuts_And_InvestUnitDatums of
                            [x] -> x
                            _   -> traceError "expected exactly one InvestUnit output"
                    ------------------
                        !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                        !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_FundDatum
                    ------------------
                        !fundClassIndex = T.fdFundClassIndex fundDatum_Out
                    ---------------------
                        !fundClasses = ProtocolT.pdFundClasses protocolDatum_In
                        !selectedFuncClass' = find (\fundClass' -> ProtocolT.fcIndex fundClass' == fundClassIndex) fundClasses
                    ---------------------
                        !requiredMAYZ = case selectedFuncClass' of
                                Nothing ->
                                    traceError "Can't find Fund Class"
                                Just selectedFuncClass -> do
                                    ProtocolT.fcRequiredMAYZ selectedFuncClass
                    ---------------------
                        !tokenMAYZ_AC = LedgerValue.AssetClass (tokenMAYZ_CS, T.tokenMAYZ_TN)
                        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
                    ---------------------
                        !valueFor_Mint_FundID = LedgerValue.assetClassValue fundID_AC 1
                        !valueFor_Mint_InvestUnitID = LedgerValue.assetClassValue investUnitID_AC 1
                    ---------------------
                        !valueFor_Mint_FundID_And_OtherIDs = valueFor_Mint_FundID <> valueFor_Mint_InvestUnitID
                    ---------------------
                        !valueFor_FundDatum' = valueFor_Mint_FundID
                        !minADA_For_FundDatum = OnChainHelpers.calculateMinADAOfValue valueFor_FundDatum' True
                        !value_MinADA_For_FundDatum = LedgerAda.lovelaceValueOf minADA_For_FundDatum
                        !valueFor_FundDatum_Out_Control = valueFor_FundDatum' <> value_MinADA_For_FundDatum <> valueOf_RequiredMAYZ <> LedgerAda.lovelaceValueOf 5_000_000 -- TODO: min ada para datum grande
                    ---------------------
                        !valueFor_InvestUnitDatum' = valueFor_Mint_InvestUnitID
                        !minADA_For_InvestUnitDatum = OnChainHelpers.calculateMinADAOfValue valueFor_InvestUnitDatum' True
                        !value_MinADA_For_InvestUnitDatum = LedgerAda.lovelaceValueOf minADA_For_InvestUnitDatum
                        !valueFor_InvestUnitDatum_Out_Control = valueFor_InvestUnitDatum' <> value_MinADA_For_InvestUnitDatum <> LedgerAda.lovelaceValueOf 5_000_000 -- TODO: min ada para datum grande
                    ---------------------
                        --TODO: controlar todos los campos del fund datum con los valores de rangos min max de protocol datum
                    ---------------------
                        !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_Out
                        !fundHoldingValidator_Hash = T.fdFundHoldingValidator_Hash fundDatum_Out
                        !investUnitValidator_Hash = T.fdInvestUnitValidator_Hash fundDatum_Out
                    ---------------------
                        !admins = T.fdAdmins fundDatum_Out
                        !fundClass = T.fdFundClassIndex fundDatum_Out
                        !beginAt = T.fdBeginAt fundDatum_Out
                        !deadline = T.fdDeadline fundDatum_Out
                        !closedAt = T.fdClosedAt fundDatum_Out
                        !commission = T.fdCommissionsPerYearInBPx1e3 fundDatum_Out
                        !holdingsCount = 0
                        !holdingsIndex = 0
                        !holdingsCreators = []
                    ---------------------
                        !fundDatum_Out_Control = T.mkFundDatumType
                            fundPolicy_CS
                            fundValidator_Hash
                            fundHoldingPolicyID_CS
                            fundHoldingValidator_Hash
                            investUnitValidator_Hash
                            admins fundClass beginAt deadline closedAt commission holdingsCount holdingsIndex holdingsCreators minADA_For_FundDatum
                    ---------------------
                        -- TODO: quiero controlar estos otros datums?
                        -- !investUnit = pfppInvestUnit
                        -- !investUnitDatum_Out = InvestUnitT.mkInvestUnitDatum fundPolicy_CS investUnit minADA_For_InvestUnitDatum
                    -----------------
                        isMintingIDs :: Bool
                        isMintingIDs = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_FundID_And_OtherIDs
                    -----------------
                        isCorrect_Outputs_Addresses :: Bool
                        isCorrect_Outputs_Addresses =
                            let
                            ------------------
                                !fundDatum_Address = OnChainHelpers.getAddress_In_TxOut_And_Datum output_TxOut_And_FundDatum
                                !investUnitDatum_Address = OnChainHelpers.getAddress_In_TxOut_And_Datum output_TxOut_And_InvestUnitDatum
                            ------------------
                                !fundDatum_Address_Hash = OnChainHelpers.getUnsafeScriptHash_In_Address fundDatum_Address
                                !investUnitDatum_Address_Hash = OnChainHelpers.getUnsafeScriptHash_In_Address investUnitDatum_Address
                            ------------------
                            in fundDatum_Address_Hash == fundValidator_Hash
                                && investUnitDatum_Address_Hash == investUnitValidator_Hash
                    ------------------
                        isCorrect_Output_FundDatum :: Bool
                        isCorrect_Output_FundDatum =
                            fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
                    ------------------
                        isCorrect_Output_FundDatum_Value :: Bool
                        isCorrect_Output_FundDatum_Value =
                            -- TODO simplificar buscando nft
                            -- TODO: en este caso tambien esta controlando que esten los MAYZ. si busco NFT tendria que controlar lo de los MAYZ aparte
                            let !valueOf_FundDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_FundDatum
                            in  valueOf_FundDatum_Out `OnChainHelpers.isEqValue` valueFor_FundDatum_Out_Control
                    ------------------
                        isCorrect_Output_InvestUnitDatum_Value :: Bool
                        isCorrect_Output_InvestUnitDatum_Value =
                            let !valueOf_InvestUnitDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_InvestUnitDatum
                            in  valueOf_InvestUnitDatum_Out `OnChainHelpers.isEqValue` valueFor_InvestUnitDatum_Out_Control
                    ------------------
                (T.PolicyRedeemerBurnID _) ->
                    -- Que se consuma ProtocolDatum con redeemer correcto (DeleteFund)
                    -- Para identificar el correcto ProtocolDatum necesita la póliza Protocol ID que está en los parámetros de esta póliza.
                    -- Que se quemen todos los Fund IDs con own póliza
                    -- No hay control adicional sobre los MAYZ, se supone que quien hace la tx, es un Fund Admin y el recupera los MAYZ o los envia a cualquier lado
                    ---------------------
                        traceIfFalse "not isBurningIDs" isBurningIDs
                    ---------------------
                    where
                     ------------------
                        !valueFor_Burn_FundID = LedgerValue.assetClassValue fundID_AC (negate 1)
                        !valueFor_Burn_InvestUnitID = LedgerValue.assetClassValue investUnitID_AC (negate 1)
                    ---------------------
                        !valueFor_Burn_FundID_And_OtherIDs = valueFor_Burn_FundID <> valueFor_Burn_InvestUnitID
                    ---------------------
                        isBurningIDs :: Bool
                        isBurningIDs = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_FundID_And_OtherIDs
                    -----------------
                _ -> False
            where
            ------------------
                !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
            ------------------

    ------------------
        validateMintAndBurnFT :: Bool
        validateMintAndBurnFT =
            case redeemer of
                    (T.PolicyRedeemerMintFT _) ->
                        -- que se este consumiendo fundholding datum con el redeemer correcto
                        -- que se este minteando el FT
                        ------------------
                            traceIfFalse "not isMintingFT" isMintingFT &&
                            traceIfFalse "not isCorrect_Redeemer_FundHoldingDatum" (isCorrect_Redeemer_FundHoldingDatum isFundHoldingValidatorRedeemerDeposit)
                    (T.PolicyRedeemerBurnFT _) ->
                        -- que se este consumiendo fundholding datum con el redeemer correcto
                        -- que se este quemando el FT
                        ------------------
                            traceIfFalse "not isBurningFT" isBurningFT &&
                            traceIfFalse "not isCorrect_Redeemer_FundHoldingDatum" (isCorrect_Redeemer_FundHoldingDatum isFundHoldingValidatorRedeemerWithdraw)
                    _ -> False
            ------------------
            where
            ------------------
                isFundHoldingValidatorRedeemerDeposit :: FundHoldingT.ValidatorRedeemer -> Bool
                isFundHoldingValidatorRedeemerDeposit redemeerToCheck = case redemeerToCheck of
                    FundHoldingT.ValidatorRedeemerDeposit _ -> True
                    _                                       -> False
            ------------------
                isFundHoldingValidatorRedeemerWithdraw :: FundHoldingT.ValidatorRedeemer -> Bool
                isFundHoldingValidatorRedeemerWithdraw redemeerToCheck = case redemeerToCheck of
                    FundHoldingT.ValidatorRedeemerWithdraw _ -> True
                    _                                        -> False
            ------------------
                isCorrect_Redeemer_FundHoldingDatum :: (FundHoldingT.ValidatorRedeemer -> Bool) -> Bool
                isCorrect_Redeemer_FundHoldingDatum isRedeemerType  =
                    let
                    ------------------
                        -- !fundPolicy_CS = LedgerContextsV2.ownCurrencySymbol ctx
                        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                    ------------------
                        !inputs_TxOuts_And_FundDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @T.ValidatorDatum @T.FundDatumType ctx fundID_AC T.getFundDatumType
                        !input_TxOut_And_FundDatum = case inputs_TxOuts_And_FundDatums of
                            [x] -> x
                            _   -> traceError "expected exactly one Fund input ref"
                    ------------------
                        !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
                    ------------------
                        !fundHoldingID_CS = T.fdFundHoldingPolicyID_CS fundDatum_In
                    ------------------
                        -- TODO: un layer se seguridad puede ser guardar el address de donde deberia estar esta input -> no hace falta, confio en ID
                        !inputs_TxOutRef_TxOut_And_FundHoldingDatums = OnChainHelpers.getUnsafe_TxOutRefs_TxOuts_And_DatumTypes_from_Inputs_By_CS @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType ctx fundHoldingID_CS FundHoldingT.getFundHoldingDatumType
                        !input_TxOutRef_TxOut_And_FundHoldingDatum = case inputs_TxOutRef_TxOut_And_FundHoldingDatums of
                                [x] -> x
                                _   -> traceError "expected exactly one FundHolding input"
                    ------------------
                        !redeemerFor_FundHoldingDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_FundHoldingDatum) info
                    in case redeemerFor_FundHoldingDatum' of
                                Nothing -> False
                                Just redeemerFor_FundHoldingDatum ->
                                    case LedgerApiV2.fromBuiltinData @FundHoldingT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundHoldingDatum of
                                        Just x -> isRedeemerType x
                                        _      -> False
            ------------------
                isMintingFT :: Bool
                isMintingFT =
                    let !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
                    in OnChainHelpers.isToken_Minting_With_AC fundFT_AC  info
            ------------------
                isBurningFT :: Bool
                isBurningFT =
                    let !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
                    in OnChainHelpers.isToken_Burning_With_AC fundFT_AC  info
    ------------------

--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator T.ValidatorParams {..} !datumRaw !redRaw !ctxRaw =
    if
        traceIfFalse "" useThisToMakeScriptUnique
        && validateAdminAction (T.fdAdmins fundDatum_In)
        && validateRedeemer
        then () else error ()
    where
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
        !protocolPolicyID_CS = vpProtocolPolicyID_CS
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
    ------------------
        !input_Current_TxOut = OnChainHelpers.getUnsafe_Current_Input_TxOut ctx
    ------------------
        !fundDatum_In = T.getFundDatumType datum
    ------------------
        !fundPolicy_CS = T.fdFundPolicy_CS fundDatum_In -- vpPolicyID_CS  --
    ------------------
        validateRedeemer :: Bool
        validateRedeemer = case redeemer of
            (T.ValidatorRedeemerDatumUpdate redeemerType) ->
                   traceIfFalse "not isCorrect_Output_FundDatum_Updated" (isCorrect_Output_FundDatum_Updated redeemerType)
                && traceIfFalse "not isCorrect_Output_FundDatum_Value_NotChanged" isCorrect_Output_FundDatum_Value_NotChanged
            (T.ValidatorRedeemerFundHoldingAdd T.ValidatorRedeemerFundHoldingAddType {..}) ->
                -- Que sea Fund Admin
                -- Que se mintee Holding ID con la correcta póliza indicada en FundDatum
                -- Que el FundDatum regrese a Fund Val
                -- Que el FundDatum se actualiza con nuevo Holding
                -- Que el FundDatum value no cambie
                let
                    !creator = rhaCreator
                    !creatorStakeCredential = rhaCreatorStakeCredential
                    !minADA = rhaMinADA
                in     traceIfFalse "not isCorrect_Output_FundDatum_With_HoldingAdded" (isCorrect_Output_FundDatum_With_HoldingAdded creator creatorStakeCredential minADA)
                    && traceIfFalse "not isCorrect_Output_FundDatum_Value_NotChanged" isCorrect_Output_FundDatum_Value_NotChanged
                    && traceIfFalse "not isMintingFundHoldingID" isMintingFundHoldingID
            (T.ValidatorRedeemerFundHoldingDelete T.ValidatorRedeemerFundHoldingDeleteType) ->
                -- Que sea Fund Admin
                -- Que se queme Holding ID con la correcta póliza indicada en FundDatum
                -- Que el FundDatum regrese a Fund Val
                -- Que el FundDatum se actualiza con el Holding eliminado
                -- Que el FundDatum value no cambie
                   traceIfFalse "not isCorrect_Output_FundDatum_With_HoldingDeleted" isCorrect_Output_FundDatum_With_HoldingDeleted
                && traceIfFalse "not isCorrect_Output_FundDatum_Value_NotChanged" isCorrect_Output_FundDatum_Value_NotChanged
                && traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
            (T.ValidatorRedeemerDelete _) ->
                -- Que sea Fund Admin
                -- Que se quemen todos los Fund IDs con la correcta póliza indicada en FundDatum siendo consumido (no hace falta controlar que se quemen todos, con que se queme Fund ID esta bien)
                traceIfFalse "not isBurningFundID" isBurningFundID
    ------------------
        getOutput_Own_TxOut_And_FundDatum :: (LedgerApiV2.TxOut, T.FundDatumType)
        getOutput_Own_TxOut_And_FundDatum =
            let
                !outputs_Own_TxOuts = OnChainHelpers.getUnsafe_Own_Outputs_TxOuts ctx
            ------------------
                !outputs_Own_TxOuts_And_FundDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.FundDatumType outputs_Own_TxOuts ctx fundPolicy_CS T.getFundDatumType
                !output_Own_TxOut_And_FundDatum = case outputs_Own_TxOuts_And_FundDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one Fund output"
            ------------------
            in output_Own_TxOut_And_FundDatum
    ------------------
        validateAdminAction ::
            [T.WalletPaymentPKH] ->
            Bool
        validateAdminAction !admins =
               traceIfFalse "not isSignedByAny admins" (OnChainHelpers.isSignedByAny admins info)
            && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
    ------------------
        isCorrect_Output_FundDatum_Updated :: T.ValidatorRedeemerDatumUpdateType -> Bool
        isCorrect_Output_FundDatum_Updated T.ValidatorRedeemerDatumUpdateType =
            let !output_Own_TxOut_And_FundDatum = getOutput_Own_TxOut_And_FundDatum
            ------------------
                !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                !fundDatum_Out_Control = OnChainHelpers.mkUpdated_FundDatum_With_NormalChanges fundDatum_In (T.fdAdmins fundDatum_Out)
            in  fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
    ------------------
        isCorrect_Output_FundDatum_With_HoldingAdded ::  T.WalletPaymentPKH -> Maybe T.StakeCredentialPubKeyHash -> Integer -> Bool
        isCorrect_Output_FundDatum_With_HoldingAdded !creator !creatorStakeCredential !minADA =
            let !output_Own_TxOut_And_FundDatum = getOutput_Own_TxOut_And_FundDatum
            ------------------
                !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                !fundDatum_Out_Control = OnChainHelpers.mkUpdated_FundDatum_With_HoldingAdded fundDatum_In creator  creatorStakeCredential minADA
            in  fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
    ------------------
        isCorrect_Output_FundDatum_With_HoldingDeleted :: Bool
        isCorrect_Output_FundDatum_With_HoldingDeleted =
            let !output_Own_TxOut_And_FundDatum = getOutput_Own_TxOut_And_FundDatum
            ------------------
                !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                !fundDatum_Out_Control = OnChainHelpers.mkUpdated_FundDatum_With_HoldingDeleted fundDatum_In
            in  fundDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundDatum_Out_Control
    ------------------
        isCorrect_Output_FundDatum_Value_NotChanged :: Bool
        isCorrect_Output_FundDatum_Value_NotChanged =
            let !output_Own_TxOut_And_FundDatum = getOutput_Own_TxOut_And_FundDatum
            ------------------
                !valueOf_FundDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundDatum
                !valueFor_FundDatum_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut
            in  valueOf_FundDatum_Out `OnChainHelpers.isEqValue` valueFor_FundDatum_Out_Control
    ------------------
        isMintingFundHoldingID :: Bool
        isMintingFundHoldingID =
            let !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_In
            in OnChainHelpers.isNFT_Minting_With_CS fundHoldingPolicyID_CS info
    ------------------
        isBurningFundHoldingID :: Bool
        isBurningFundHoldingID =
            let !fundHoldingPolicyID_CS = T.fdFundHoldingPolicyID_CS fundDatum_In
            in OnChainHelpers.isNFT_Burning_With_CS fundHoldingPolicyID_CS info
    ------------------
        isBurningFundID :: Bool
        isBurningFundID  =
            let !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
            in OnChainHelpers.isNFT_Burning_With_AC fundID_AC info

--------------------------------------------------------------------------------2

{-# INLINEABLE policy #-}
policy :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policy params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicy||])
                    `PlutusTx.applyCode` PlutusTx.liftCode T.tokenMAYZ_CS
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINABLE  mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID tokenMAYZ_CS protocolPolicyID_CS fundPolicy_TxHash fundPolicy_TxOutputIndex fundValidator_Hash = mkPolicy (PlutusTx.unsafeFromBuiltinData tokenMAYZ_CS) params
    where
        tid = PlutusTx.unsafeFromBuiltinData fundPolicy_TxHash :: BuiltinByteString
        txout = LedgerApiV2.TxOutRef {
            LedgerApiV2.txOutRefId = LedgerApiV2.TxId tid,
            LedgerApiV2.txOutRefIdx = PlutusTx.unsafeFromBuiltinData fundPolicy_TxOutputIndex
        }
        params = T.PolicyParams
            { ppProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
             ,ppFundPolicy_TxOutRef = txout
             , ppFundValidator_Hash = PlutusTx.unsafeFromBuiltinData  fundValidator_Hash
            }

policyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCode = $$( PlutusTx.compile [|| mkWrappedPolicyID ||])


--------------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS  = mkValidator params
    where
        params = T.ValidatorParams
            { vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
            }

validatorCode :: CompiledCode ( BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])

--------------------------------------------------------------------------------2
