{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
----------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
----------------------------------------------------------------------------2
module Protocol.Fund.Holding.OnChain where

----------------------------------------------------------------------------2
-- Import Externos
----------------------------------------------------------------------------2

import qualified Data.List                   as DataList
import qualified Generic.Constants           as T
import qualified Generic.OnChainHelpers      as Helpers
import qualified Generic.OnChainHelpers      as OnChainHelpers
import qualified Generic.Types               as T
import qualified Ledger.Ada                  as LedgerAda
import qualified Ledger.Value                as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts   as LedgerContextsV2
import           PlutusTx                    (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap           as TxAssocMap
import           PlutusTx.Prelude            (AdditiveGroup ((-)), AdditiveSemigroup ((+)), Bool (..), BuiltinData, Eq ((==)), Integer, Maybe (Just, Nothing), MultiplicativeSemigroup ((*)), Ord ((>=)), Semigroup ((<>)), error, foldl, negate,
                                              otherwise, sum, traceError, traceIfFalse, traceIfTrue, ($), (&&), (++), (.), (/=), (<$>), (||))
import qualified PlutusTx.Ratio              as TxRatio

----------------------------------------------------------------------------2
-- Import Internos
----------------------------------------------------------------------------2

import qualified Protocol.Constants          as T
import qualified Protocol.Fund.Helpers       as Helpers
import qualified Protocol.Fund.Holding.Types as T
import qualified Protocol.Fund.Types         as FundT
import qualified Protocol.InvestUnit.Types   as InvestUnitT
import qualified Protocol.Protocol.Types     as ProtocolT
import qualified Protocol.Types              as T

----------------------------------------------------------------------------2
-- Modulo
----------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID T.PolicyParams {..} !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        &&
        case redeemer of
        (T.PolicyRedeemerMintID _) ->
            -- Que se consuma FundDatum con redeemer correcto (AddHolding)
            -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
            -- Que se genere salida con nuevo HoldingDatum en Holding Val (Holding Val está indicada en FundDatum)
            -- Que el HoldingDatum sea correcto
            -- Que se mintee Holding ID con own póliza
            -- Que el HoldingDatum tenga el Holding ID
            ------------------
            traceIfTrue "isThisPolicyDifferent" isThisPolicyDifferent &&
            traceIfFalse "not isMintingFundHoldingID" isMintingFundHoldingID &&
            traceIfFalse "not isCorrect_Redeemer_FundDatum" (isCorrect_Redeemer_FundDatum isFundValidatorRedeemerFundHoldingAdd ) &&
            traceIfFalse "not isCorrect_Outputs_Addresses" isCorrect_Outputs_Addresses &&
            traceIfFalse "not isCorrect_Output_FundHoldingDatum" isCorrect_Output_FundHoldingDatum &&
            traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value" isCorrect_Output_FundHoldingDatum_Value
            where
                !cs = LedgerContextsV2.ownCurrencySymbol ctx
            ------------------
                !input_TxOut_And_FundDatum = (\(_, txOut, datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_FundDatum
            ------------------
                !fundDatum_In = Helpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
            ------------------
                !fundHolding_Index = FundT.fdHoldingsIndex fundDatum_In
                !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
            ------------------
                !outputs_TxOuts_And_FundHoldingDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_AC @T.ValidatorDatum @T.FundHoldingDatumType ctx fundHoldingID_AC T.getFundHoldingDatumType
                !output_TxOut_And_FundHoldingDatum = case outputs_TxOuts_And_FundHoldingDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one FundHolding output"
            ------------------
                !fundHoldingDatum_Out = Helpers.getDatum_In_TxOut_And_Datum output_TxOut_And_FundHoldingDatum
            ------------------
                !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
            ------------------
                !valueFor_FundHoldingDatum_Out' = valueFor_Mint_FundHoldingID
                !minADA_For_FundHoldingDatum_Out = OnChainHelpers.calculateMinADAOfValue valueFor_FundHoldingDatum_Out' True
                !value_MinADA_For_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_Out
                !valueFor_FundHoldingDatum_Out_Control = valueFor_FundHoldingDatum_Out' <> value_MinADA_For_FundHoldingDatum_Out <> LedgerAda.lovelaceValueOf 10000000
                -- TODO: min ada paratodos los tokens quje tendra
            ------------------
                !fundHoldingDatum_Out_Control = T.FundHoldingDatumType  {
                                T.hdFundHolding_Index = fundHolding_Index,
                                T.hdSubtotal_FT_Minted_Accumulated = 0,
                                T.hdSubtotal_FT_Minted = 0,
                                T.hdSubtotal_FT_Circulation = 0,
                                T.hdSubtotal_FT_ForComission = 0,
                                T.hdSubtotal_FT_ForComission_Acumulated = 0,
                                T.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 = 0,
                                T.hdSubtotal_Collected_Commissions_Protocol = 0,
                                T.hdSubtotal_Collected_Commissions_MAYZ = 0,
                                T.hdSubtotal_Collected_Commissions_FundAdmins = 0,
                                T.hdMinADA = minADA_For_FundHoldingDatum_Out
                            }
            ------------------
                -- Only needed to read the value of the parameter and get this policy a hash different than others
                -- If I dont use the parameter is like is not there, so the hash will be the same as others
                isThisPolicyDifferent :: Bool
                !isThisPolicyDifferent = cs /= ppFundPolicy_CS
            ------------------
                isMintingFundHoldingID :: Bool
                isMintingFundHoldingID = Helpers.getUnsafeOwnMintingValue ctx `Helpers.isEqValue` valueFor_Mint_FundHoldingID
            -----------------
                isCorrect_Outputs_Addresses :: Bool
                isCorrect_Outputs_Addresses =
                    let
                        !fundHoldingValidator_Hash = FundT.fdFundHoldingValidator_Hash fundDatum_In
                        !fundHoldingDatum_Address = Helpers.getAddress_In_TxOut_And_Datum output_TxOut_And_FundHoldingDatum
                    ------------------
                        !fundHoldingDatum_Address_Hash = Helpers.getUnsafeScriptHash_In_Address fundHoldingDatum_Address
                    ------------------
                    in fundHoldingDatum_Address_Hash == fundHoldingValidator_Hash
            ------------------
                isCorrect_Output_FundHoldingDatum :: Bool
                isCorrect_Output_FundHoldingDatum =
                    fundHoldingDatum_Out `Helpers.isUnsafeEqDatums` fundHoldingDatum_Out_Control
            ------------------
                isCorrect_Output_FundHoldingDatum_Value :: Bool
                isCorrect_Output_FundHoldingDatum_Value =
                    -- TODO simplificar buscando nft
                    let !valueOf_FundHoldingDatum_Out = Helpers.getValue_In_TxOut_And_Datum output_TxOut_And_FundHoldingDatum
                    in  valueOf_FundHoldingDatum_Out `Helpers.isEqValue` valueFor_FundHoldingDatum_Out_Control
            ------------------
        (T.PolicyRedeemerBurnID _) ->
            -- Que se consuma FundDatum con redeemer correcto (DeleteHolding)
            -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
            -- Que se queme Holding ID con own póliza
            ------------------
               traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
            && traceIfFalse "not isCorrect_Redeemer_FundDatum" (isCorrect_Redeemer_FundDatum isFundValidatorRedeemerFundHoldingDelete)
            ------------------
            where
            ------------------
                !inputs_TxOuts_And_FundHoldingDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_Inputs_By_CS @T.ValidatorDatum @T.FundHoldingDatumType ctx fundHoldingPolicyID_CS T.getFundHoldingDatumType
                !input_TxOut_And_FundHoldingDatum = case inputs_TxOuts_And_FundHoldingDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one FundHolding input"
            ------------------
                !fundHoldingDatum_In = Helpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundHoldingDatum
            ------------------
                !fundHolding_Index = T.hdFundHolding_Index fundHoldingDatum_In
                !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
            ------------------
                !valueFor_Burn_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC (negate 1)
            ------------------
                isBurningFundHoldingID :: Bool
                isBurningFundHoldingID = Helpers.getUnsafeOwnMintingValue ctx `Helpers.isEqValue` valueFor_Burn_FundHoldingID
            -----------------
    then ()
    else error ()
        where
            !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
            !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
            !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
            !fundPolicy_CS = ppFundPolicy_CS
        ------------------
            -- TODO: para hacer la politica unica con respecto al protocolo
            !useThisToMakeScriptUnique = True -- No hace falta, el parametro de la poliza ya esta siendo utilizado en algun lado
        ------------------
            !fundHoldingPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
         ------------------
            !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        ------------------
            !inputs_TxOutRef_TxOut_And_FundDatums = Helpers.getUnsafe_TxOutRefs_TxOuts_And_DatumTypes_from_Inputs_By_AC @FundT.ValidatorDatum @FundT.FundDatumType ctx fundID_AC FundT.getFundDatumType
            !input_TxOutRef_TxOut_And_FundDatum = case inputs_TxOutRef_TxOut_And_FundDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one Fund input"
        ------------------
            isFundValidatorRedeemerFundHoldingAdd :: FundT.ValidatorRedeemer -> Bool
            isFundValidatorRedeemerFundHoldingAdd redemeerToCheck = case redemeerToCheck of
                FundT.ValidatorRedeemerFundHoldingAdd _ -> True
                _                                       -> False
        ------------------
            isFundValidatorRedeemerFundHoldingDelete :: FundT.ValidatorRedeemer -> Bool
            isFundValidatorRedeemerFundHoldingDelete redemeerToCheck = case redemeerToCheck of
                FundT.ValidatorRedeemerFundHoldingDelete _ -> True
                _                                          -> False
        ------------------
            isCorrect_Redeemer_FundDatum :: (FundT.ValidatorRedeemer -> Bool) -> Bool
            isCorrect_Redeemer_FundDatum isRedeemerType =
                let !redeemerFor_FundDatum' = Helpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_FundDatum) info
                in case redeemerFor_FundDatum' of
                    Nothing -> False
                    Just redeemerFor_FundDatum -> case LedgerApiV2.fromBuiltinData @FundT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundDatum of
                        Just x -> isRedeemerType x
                        _      -> False

----------------------------------------------------------------------------2

redeemerDepositAndWithdraw :: Integer
redeemerDepositAndWithdraw = 1

redeemerReIdx :: Integer
redeemerReIdx = 2

redeemerDelete :: Integer
redeemerDelete = 3

redeemerCommissions :: Integer
redeemerCommissions = 4


{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator T.ValidatorParams {..} !datumRaw !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        &&
        validateRedeemer getRedeemerType then () else error ()
    where
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
        !protocolPolicyID_CS = vpProtocolPolicyID_CS
        !fundPolicy_CS = vpFundPolicy_CS
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- No hace falta, los dos parametros del script estan siendo utilizados en la validacion
    ------------------
        !input_Current_TxOut = Helpers.getUnsafe_Current_Input_TxOut ctx
    ------------------
        !fundHoldingDatum_In = T.getFundHoldingDatumType datum
    ------------------
        !valueOf_FundHoldingDatum_In = LedgerApiV2.txOutValue input_Current_TxOut
    ------------------
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ------------------
        validateRedeemer :: Integer -> Bool
        validateRedeemer redeemerType
            | redeemerType == redeemerDepositAndWithdraw || redeemerType == redeemerCommissions = validateAllButDelete redeemerType
            | redeemerType == redeemerReIdx  = validateAllButDelete redeemerType
            | redeemerType == redeemerDelete = validateDelete
            | otherwise = False
    ------------------
        getRedeemerType :: Integer
        getRedeemerType = case redeemer of
            (T.ValidatorRedeemerDeposit _)                        -> redeemerDepositAndWithdraw
            (T.ValidatorRedeemerWithdraw _)                       -> redeemerDepositAndWithdraw
            (T.ValidatorRedeemerReIndexing _)                     -> redeemerReIdx
            (T.ValidatorRedeemerCollect_Protocol_Commissions _)   -> redeemerCommissions
            (T.ValidatorRedeemerCollect_MAYZ_Commissions _)       -> redeemerCommissions
            (T.ValidatorRedeemerCollect_FundAdmins_Commissions _) -> redeemerCommissions
            (T.ValidatorRedeemerDelete _)                         -> redeemerDelete
    ------------------
        validateAdminAction ::
            [T.WalletPaymentPKH] ->
            Bool
        validateAdminAction !admins =
                   traceIfFalse "not isSignedByAny admins" (Helpers.isSignedByAny admins info)
                && traceIfFalse "not isValidRange" (Helpers.isValidRange info T.validTimeRange)
    ------------------
        validateDelete :: Bool
        validateDelete =
            -- Que se queme Holding ID con la correcta póliza indicada en FundDatum
            -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
            ------------------
            validateAdminAction (FundT.fdAdmins fundDatum_In) &&
            traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
            ------------------
            where
                !inputs_TxOuts_And_FundDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_Inputs_By_AC @FundT.ValidatorDatum @FundT.FundDatumType ctx fundID_AC FundT.getFundDatumType
                !input_TxOut_And_FundDatum = case inputs_TxOuts_And_FundDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one Fund input"
            ------------------
                !fundDatum_In = Helpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
            ------------------
                !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_In
            ------------------
                isBurningFundHoldingID :: Bool
                isBurningFundHoldingID = Helpers.isNFT_Burning_With_CS fundHoldingPolicyID_CS info
    ------------------
        validateAllButDelete :: Integer -> Bool
        validateAllButDelete redeemerType
            | redeemerType == redeemerDepositAndWithdraw = validateDepositAndWithdraw
            | redeemerType == redeemerReIdx = validateAdminAction (FundT.fdAdmins fundDatum_In) &&  validateReIdx
            | redeemerType == redeemerCommissions = validateCommissions
            | otherwise = False
            where
            ------------------
                !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
            ------------------
                !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_In
            ------------------
                !inputsRef_TxOuts_And_FundDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @FundT.ValidatorDatum @FundT.FundDatumType ctx fundID_AC FundT.getFundDatumType
                !inputRef_TxOut_And_FundDatum = case inputsRef_TxOuts_And_FundDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one Fund input ref"
            ------------------
                !fundDatum_In = Helpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
            ------------------
                !outputs_Own_TxOuts = Helpers.getUnsafe_Own_Outputs_TxOuts ctx
            ------------------
                !outputs_Own_TxOuts_And_FundHoldingDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.FundHoldingDatumType outputs_Own_TxOuts ctx fundHoldingPolicyID_CS T.getFundHoldingDatumType
                !output_Own_TxOut_And_FundHoldingDatum = case outputs_Own_TxOuts_And_FundHoldingDatums of
                        [x] -> x
                        _   -> traceError "expected exactly one FundHolding output"
            -------------------
                isCorrect_Output_FundHoldingDatum :: T.FundHoldingDatumType -> Bool
                isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control =
                    let !fundHoldingDatum_Out = Helpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                    in  fundHoldingDatum_Out `Helpers.isUnsafeEqDatums` fundHoldingDatum_Control
            ------------------
                isCorrect_Output_FundHoldingDatum_Value :: LedgerValue.Value -> Bool
                isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control =
                    let !valueOf_FundHoldingDatum_Out = Helpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                    in  valueOf_FundHoldingDatum_Out `Helpers.isEqValue` valueFor_FundHoldingDatum_Control
            ------------------
                validateDepositAndWithdraw :: Bool
                validateDepositAndWithdraw =
                    traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
                    && case redeemer of
                        (T.ValidatorRedeemerDeposit T.ValidatorRedeemerDepositType{..}) ->
                                   traceIfFalse "not isCorrect_Output_FundHoldingDatum_Updated_With_Deposit" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_With_Deposit)
                                && traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithTokensAndFT" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithTokensAndFT)
                                && traceIfFalse "not isMintingFT" isMintingFT
                                && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                            where
                            ------------------
                                !deposit = rdAmount
                                !date = rdDate
                                !deadline = FundT.fdDeadline fundDatum_In
                                !commissionsPerYearInBPx1e3 = FundT.fdCommissionsPerYearInBPx1e3 fundDatum_In
                            ---------------------
                                !(userFT, commissionsFT, commissions_RatePerMonth_Numerator1e6) = Helpers.calculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date deposit
                            ------------------
                                !valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value = createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value valueOf_FundHoldingDatum_In deposit
                            ------------------
                                !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
                            ------------------
                                !valueFor_FundHoldingDatum_Control_WithTokensAndFT = valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value <> valueFor_FT_Commissions -- valueOf_FundHoldingDatum_In -- <> valueOf_TokensForDeposit <> valueFor_FT_Commissions
                            ------------------
                                !fundHoldingDatum_Control_With_Deposit = Helpers.mkUpdated_FundHoldingDatum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_RatePerMonth_Numerator1e6
                            ------------------
                                isMintingFT :: Bool
                                isMintingFT = Helpers.isToken_Minting_With_AC_AndAmt fundFT_AC deposit info
                            ------------------
                        (T.ValidatorRedeemerWithdraw  T.ValidatorRedeemerWithdrawType{..}) ->
                               traceIfFalse "not isCorrect_Output_FundHoldingDatum_Updated_With_Withdraw" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_With_Withdraw)
                            && traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithoutTokensAndFTforCommissions" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions)
                            && traceIfFalse "not isBurningFT" isBurningFT
                            && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                            ------------------
                            where
                            ------------------
                                !withdraw = rwAmount
                                !date = rwDate
                            ---------------------
                                !deadline = FundT.fdDeadline fundDatum_In
                                !commissionsPerYearInBPx1e3 = FundT.fdCommissionsPerYearInBPx1e3 fundDatum_In
                            ---------------------
                                !(commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_RatePerMonth_Numerator1e6) = Helpers.calculateWithdrawComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date withdraw
                            ------------------
                                !valueOf_TokensForWithdraw_Plus_FundHoldingDatum_Value = createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value valueOf_FundHoldingDatum_In (negate withdrawPlusCommissionsGetBack)
                            ------------------
                                !valueFor_FT_CommissionsToGetBack = LedgerValue.assetClassValue fundFT_AC commissionsForUserFTToGetBack
                            ------------------
                                !valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions = valueOf_TokensForWithdraw_Plus_FundHoldingDatum_Value <> negate valueFor_FT_CommissionsToGetBack
                            ------------------
                                !fundHoldingDatum_Control_With_Withdraw  = Helpers.mkUpdated_FundHoldingDatum_With_Withdraw fundHoldingDatum_In withdraw commissionsForUserFTToGetBack commissions_RatePerMonth_Numerator1e6
                            ------------------
                                isBurningFT :: Bool
                                isBurningFT = Helpers.isToken_Minting_With_AC_AndAmt fundFT_AC (negate withdrawPlusCommissionsGetBack) info
                            ------------------
                        _ -> False
                    ------------------
                    where
                    ------------------
                        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                    ------------------
                        !inputsRef_TxOuts_And_InvestUnitDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType ctx investUnitID_AC InvestUnitT.getInvestUnitDatumType
                        !inputRef_TxOut_And_InvestUnitDatum = case inputsRef_TxOuts_And_InvestUnitDatums of
                            [x] -> x
                            _   -> traceError "expected exactly one InvestUnit input ref"
                    ------------------
                        !investUnitDatum_In = Helpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_InvestUnitDatum
                    ------------------
                        !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
                        !investUnitTokens = T.iuValues investUnit
                    ------------------
                        createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value :: LedgerValue.Value -> Integer -> LedgerValue.Value
                        createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value (LedgerValue.Value mp) amount =
                            LedgerValue.Value mapCSResult
                            where
                                !listMapCS = TxAssocMap.toList mp
                                !listTokens =  investUnitTokens --TODO: sort solo por CS
                                !mapCSResult = TxAssocMap.fromList (updateListMapCS listTokens listMapCS)
                            ------------------
                                updateListMapCS [] restListMapCS = restListMapCS
                                updateListMapCS ((cs, tn, amt): restTokens) restListMapCS  =
                                    let
                                        !(tokensFromSameCS, restTokensWithoutCS) = getOthersTokensFromSameCSAndDeleteFromList cs restTokens [] []
                                        !(mapFromSameCS, restMapWithoutCS) = getMapFromSameCSAndDeleteFromList cs restListMapCS []
                                        !mapFromSameCSWithTokensAdded = addTokensInMap cs mapFromSameCS ((tn, amt):tokensFromSameCS)
                                        !resultMap = mapFromSameCSWithTokensAdded : updateListMapCS restTokensWithoutCS restMapWithoutCS
                                    in resultMap
                            ------------------
                                getOthersTokensFromSameCSAndDeleteFromList _ [] accListFromSame accListOthers = (accListFromSame, accListOthers)
                                getOthersTokensFromSameCSAndDeleteFromList cs ((cs', tn', amt'): restTokens) accListFromSame accListOthers
                                    | cs == cs' = getOthersTokensFromSameCSAndDeleteFromList cs restTokens ( (tn', amt'): accListFromSame) accListOthers
                                    | otherwise = getOthersTokensFromSameCSAndDeleteFromList cs restTokens accListFromSame ( (cs', tn', amt'): accListOthers)
                            ------------------
                                getMapFromSameCSAndDeleteFromList _ [] accListMapOthers = (Nothing, accListMapOthers)
                                getMapFromSameCSAndDeleteFromList cs ((cs', mapTN): restMap) accListOthers
                                    | cs == cs' = (Just mapTN, restMap ++ accListOthers)
                                    | otherwise = getMapFromSameCSAndDeleteFromList cs restMap ( (cs', mapTN): accListOthers)
                            ------------------
                                addTokensInMap cs Nothing tokens      = addTokensInMap' cs TxAssocMap.empty tokens
                                addTokensInMap cs (Just mapTN) tokens = addTokensInMap' cs mapTN tokens
                            ------------------
                                addTokensInMap' cs mapTN []                   = (cs, mapTN)
                                addTokensInMap' cs mapTN listTokensToAddInMap = (cs, foldl (\acc (tn', amt') -> mapElement acc tn' (amt' * amount)) mapTN listTokensToAddInMap)
                            ------------------
                                mapElement acc tn amt =
                                    case TxAssocMap.lookup tn acc of
                                        Nothing   -> TxAssocMap.insert tn amt acc
                                        Just amt' -> TxAssocMap.insert tn (amt + amt') acc
                            ------------------
                validateReIdx :: Bool
                validateReIdx =  case redeemer of
                    (T.ValidatorRedeemerReIndexing T.ValidatorRedeemerReIndexingType {..}) ->
                               traceIfFalse "not isCorrect_Output_FundHoldingDatum_NotChanged" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_NotChanged)
                            && traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithTokensExchanged" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithTokensExchanged)
                            && traceIfFalse "not isCorrect_Redeemer_InvestUnitDatum" isCorrect_Redeemer_InvestUnitDatum
                        ------------------
                        where
                        ------------------
                            !investUnitTokensToAdd = rriTokensToAdd
                            !investUnitTokensToRemove = rriTokensToRemove
                        ------------------
                            !inputsRef_TxOuts_And_FundHoldingDatums' = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_CS @T.ValidatorDatum @T.FundHoldingDatumType ctx fundHoldingPolicyID_CS T.getFundHoldingDatumType
                        ------------------
                            !fundHoldingsCount = FundT.fdHoldingsCount fundDatum_In
                        ------------------
                            !allFundHolingsIndex =  T.hdFundHolding_Index fundHoldingDatum_In : [ T.hdFundHolding_Index  dat | (_, dat) <- inputsRef_TxOuts_And_FundHoldingDatums']
                            !countDistinctFundHolingsIndex = OnChainHelpers.countDistinct allFundHolingsIndex
                        ------------------
                            !inputsRef_TxOuts_And_FundHoldingDatums =
                                if countDistinctFundHolingsIndex == fundHoldingsCount
                                    then inputsRef_TxOuts_And_FundHoldingDatums'
                                    else traceError "expected all but one FundHolding as input ref"
                        ------------------
                            !tokensToAdd = T.iuValues investUnitTokensToAdd
                            !tokensToRemove = T.iuValues investUnitTokensToRemove
                        ------------------
                            !totalDepositsIU = T.hdSubtotal_FT_Minted fundHoldingDatum_In + sum (T.hdSubtotal_FT_Minted . Helpers.getDatum_In_TxOut_And_Datum <$> inputsRef_TxOuts_And_FundHoldingDatums)
                        ------------------
                            !valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, am * totalDepositsIU) | (cs, tn, am) <- tokensToAdd ]
                            !valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, am * totalDepositsIU) | (cs, tn, am) <- tokensToRemove  ]
                        ------------------
                            !valueFor_FundHoldingDatum_Control_WithTokensExchanged  = valueOf_FundHoldingDatum_In <> valueOf_TotalTokensToAdd <> negate valueOf_TotalTokensToRemove
                        ------------------
                            !fundHoldingDatum_Control_NotChanged = fundHoldingDatum_In
                        ------------------
                            isCorrect_Redeemer_InvestUnitDatum :: Bool
                            isCorrect_Redeemer_InvestUnitDatum  =
                                let
                                ------------------
                                    !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                                ------------------
                                    !inputs_TxOutRefs_TxOuts_And_InvestUnitDatums = Helpers.getUnsafe_TxOutRefs_TxOuts_And_DatumTypes_from_Inputs_By_AC @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType ctx investUnitID_AC InvestUnitT.getInvestUnitDatumType
                                    !input_TxOutRef_TxOut_And_InvestUnitDatum = case inputs_TxOutRefs_TxOuts_And_InvestUnitDatums of
                                        [x] -> x
                                        _   -> traceError "expected exactly one InvestUnit input"
                                ------------------
                                    !redeemerFor_InvestUnitDatum' = Helpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_InvestUnitDatum) info
                                ------------------
                                in case redeemerFor_InvestUnitDatum' of
                                        Nothing -> False
                                        Just redeemerFor_InvestUnitDatum ->
                                            case LedgerApiV2.fromBuiltinData @InvestUnitT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_InvestUnitDatum of
                                                Just (InvestUnitT.ValidatorRedeemerReIndexing InvestUnitT.ValidatorRedeemerReIndexingType {..}) ->
                                                    riuriTokensToAdd == rriTokensToAdd && riuriTokensToRemove == rriTokensToRemove
                                                _ -> False
                        ------------------
                    _ -> False
                ------------------
                validateCommissions :: Bool
                validateCommissions =
                    traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
                    && case redeemer of
                        (T.ValidatorRedeemerCollect_Protocol_Commissions T.ValidatorRedeemerCollect_Protocol_CommissionsType {..} ) ->
                            traceIfFalse    "not isCorrect_Output_FundHoldingDatum_Updated_With_Collect_Protocol_Commissions" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_With_Collect_Protocol_Commissions)
                            && traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithoutTokensAndFTforCommissions" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions)
                            && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable available withdraw)
                            && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                            ------------------
                            where
                            ------------------
                                !withdraw = rwpcAmount
                                !date = rwpcDate
                            ------------------
                                !deadline = FundT.fdDeadline fundDatum_In
                            ------------------
                                !share = ProtocolT.pdShare_Protocol protocolDatum_In
                                !taken = T.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In
                                !available = Helpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken date
                             ------------------
                                !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
                            ------------------
                                !valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions = valueOf_FundHoldingDatum_In <> negate valueFor_FT_Commissions
                            ------------------
                                !fundHoldingDatum_Control_With_Collect_Protocol_Commissions = Helpers.mkUpdated_FundHoldingDatum_With_Collect_Protocol_Commissions fundHoldingDatum_In withdraw
                            ------------------
                        (T.ValidatorRedeemerCollect_MAYZ_Commissions  T.ValidatorRedeemerCollect_MAYZ_CommissionsType {..}) ->
                            traceIfFalse "not isCorrect_Output_FundHoldingDatum_Updated_With_Collect_MAYZ_Commissions" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_With_Collect_MAYZ_Commissions)
                            && traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithoutTokensAndFTforCommissions" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions)
                            && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable available withdraw)
                            && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                            ------------------
                            where
                            ------------------
                                !withdraw = rwmcAmount
                                !date = rwmcDate
                             ------------------
                                !deadline = FundT.fdDeadline fundDatum_In
                            ------------------
                                !share = ProtocolT.pdShare_MAYZ protocolDatum_In
                                !taken = T.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In
                                !available = Helpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken date
                             ------------------
                                !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
                            ------------------
                                !valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions = valueOf_FundHoldingDatum_In <> negate valueFor_FT_Commissions
                            ------------------
                                !fundHoldingDatum_Control_With_Collect_MAYZ_Commissions = Helpers.mkUpdated_FundHoldingDatum_With_Collect_MAYZ_Commissions fundHoldingDatum_In withdraw
                            ------------------
                        (T.ValidatorRedeemerCollect_FundAdmins_Commissions T.ValidatorRedeemerCollect_FundAdmins_CommissionsType {..}) ->
                            traceIfFalse "not isCorrect_Output_FundHoldingDatum_Updated_With_Collect_FundAdmins_Commissions" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_With_Collect_FundAdmins_Commissions)
                            && traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithoutTokensAndFTforCommissions" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions)
                            && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable available withdraw)
                            && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
                            ------------------
                            where
                            ------------------
                                !withdraw = rwfcAmount
                                !date =  rwfcDate
                             ------------------
                                !deadline = FundT.fdDeadline fundDatum_In
                            ------------------
                                !share = ProtocolT.pdShare_FundAdmins protocolDatum_In
                                !taken = T.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In
                                !available = Helpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken date
                            ------------------
                                !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
                            ------------------
                                !valueFor_FundHoldingDatum_Control_WithoutTokensAndFTforCommissions = valueOf_FundHoldingDatum_In <> negate valueFor_FT_Commissions
                            ------------------
                                !fundHoldingDatum_Control_With_Collect_FundAdmins_Commissions = Helpers.mkUpdated_FundHoldingDatum_With_Collect_FundAdmins_Commissions fundHoldingDatum_In withdraw
                            ------------------
                        _ -> False
                    ------------------
                    where
                    ------------------
                        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                    ------------------
                        !inputsRef_TxOuts_And_ProtocolDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType ctx protocolID_AC ProtocolT.getProtocolDatumType
                        !inputRef_TxOut_And_ProtocolDatum = case inputsRef_TxOuts_And_ProtocolDatums of
                            [x] -> x
                            _   -> traceError "expected exactly one Protocol input ref"
                    ------------------
                        !protocolDatum_In = Helpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                    ------------------
                        isCommissionsAvailable :: Integer -> Integer -> Bool
                        isCommissionsAvailable available withdraw = available >= withdraw
                    ------------------

----------------------------------------------------------------------------2

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINABLE  mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData ->  BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID fundPolicy_CS = mkPolicyID params
    where
        params = T.PolicyParams
            { ppFundPolicy_CS  = PlutusTx.unsafeFromBuiltinData fundPolicy_CS
            }

policyIDCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = $$( PlutusTx.compile [|| mkWrappedPolicyID ||])

----------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS fundPolicy_CS = mkValidator params
    where
        params = T.ValidatorParams
            {
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
            vpFundPolicy_CS  = PlutusTx.unsafeFromBuiltinData fundPolicy_CS
            }

validatorCode :: CompiledCode ( BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])

----------------------------------------------------------------------------2
