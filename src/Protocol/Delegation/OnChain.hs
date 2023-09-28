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
module Protocol.Delegation.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import           Data.Aeson                (Value (Bool))
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants         as T
import qualified Generic.OnChainHelpers    as OnChainHelpers
import qualified Generic.Types             as T
import qualified Protocol.Constants        as T
import qualified Protocol.Delegation.Types as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID ::  T.CS -> T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID  tokenMAYZ_CS T.PolicyParams{..} !redRaw !ctxRaw =
    let !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    ------------------
        !protocolPolicyID_CS = ppProtocolPolicyID_CS
    ------------------
        !delegation_Validator_Hash = ppDelegation_Validator_Hash
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
     ------------------
        !delegationPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
    ------------------
        !valueFor_Mint_Delegation_ID = LedgerValue.assetClassValue delegation_ID_AC 1
    ---------------------
        !delegation_ID_AC = LedgerValue.AssetClass (delegationPolicyID_CS, T.delegationID_TN)
    ------------------
    in  if traceIfFalse "" useThisToMakeScriptUnique
            &&
            case redeemer of
                T.PolicyRedeemerCreateDelegationID _ ->
                    -- que se mintee ID de Delegation, con esta poliza, 1 unidad, con nombre de token que venga en datum del protocolo
                    -- que vaya a la direccion del contrato correcta. La direccion puede estar en el datum del protocolo
                    -- que tenga el value correcto, con ID, con MAYZ delegados y con min ADA, segun Datum. De esta forma tambien se valida el Datum un poco
                    traceIfFalse "not isMintingDelegationID" isMintingDelegationID &&
                    traceIfFalse "not isCorrect_Output_Address" isCorrect_Output_Address &&
                    traceIfFalse "not isCorrect_Output_Delegation_Datum" isCorrect_Output_Delegation_Datum &&
                    traceIfFalse "not isCorrect_Output_Delegation_Value" isCorrect_Output_Delegation_Value
                    ---------------------
                    where
                    ------------------
                        !outputs_TxOuts_And_Delegation_Datums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_AC @T.ValidatorDatum @T.Delegation_DatumType ctx delegation_ID_AC T.getDelegation_DatumType
                        !output_TxOut_And_Delegation_Datum = case outputs_TxOuts_And_Delegation_Datums of
                            [x] -> x
                            _   -> traceError "expected exactly one Delegation output"
                    ---------------------
                        !delegation_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_Delegation_Datum
                    ---------------------
                        isMintingDelegationID :: Bool
                        isMintingDelegationID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_Delegation_ID
                    -----------------
                        isCorrect_Output_Address :: Bool
                        isCorrect_Output_Address =
                            let
                            ------------------
                                !delegation_Datum_Address = OnChainHelpers.getAddress_In_TxOut_And_Datum output_TxOut_And_Delegation_Datum
                            ------------------
                                !delegation_Datum_Address_Hash = OnChainHelpers.getUnsafeScriptHash_In_Address delegation_Datum_Address
                            ------------------
                            in delegation_Datum_Address_Hash == delegation_Validator_Hash
                     ------------------
                        isCorrect_Output_Delegation_Datum :: Bool
                        isCorrect_Output_Delegation_Datum =
                             -- TODO controlar que bodOfferedCommission_Rate_InBPx1e3 este dentro del rango permitido en el protocolo
                            let !delegation_Datum_Out_Control =
                                    T.mkDelegation_DatumType
                                        delegationPolicyID_CS
                                        (T.ddFundPolicy_CS delegation_Datum_Out)
                                        (T.ddDelegatorPaymentPKH delegation_Datum_Out)
                                        (T.ddDelegatorStakePKH delegation_Datum_Out)
                                        (T.ddDelegated_Mayz delegation_Datum_Out)
                                        (T.ddMinADA delegation_Datum_Out)
                            in  delegation_Datum_Out `OnChainHelpers.isUnsafeEqDatums` delegation_Datum_Out_Control
                    ------------------
                        isCorrect_Output_Delegation_Value :: Bool
                        isCorrect_Output_Delegation_Value =
                            let
                                !valueFor_Delegation_Datum' = valueFor_Mint_Delegation_ID
                            ---------------------
                                !minADA_For_Delegation_Datum = T.ddMinADA delegation_Datum_Out
                                -- !minADA_For_Delegation_Datum = OnChainHelpers.calculateMinADAOfValue valueFor_Delegation_Datum' True
                                !value_MinADA_For_Delegation_Datum = LedgerAda.lovelaceValueOf minADA_For_Delegation_Datum
                            ---------------------
                                !tokenMAYZ_AC = LedgerValue.AssetClass (tokenMAYZ_CS, T.tokenMAYZ_TN)
                                !delegated_MAYZ = T.ddDelegated_Mayz delegation_Datum_Out
                                !valueOf_Delegated_MAYZ = LedgerValue.assetClassValue tokenMAYZ_AC  delegated_MAYZ
                            ---------------------
                                !valueFor_Delegation_Datum_Out_Control = valueFor_Delegation_Datum' <> value_MinADA_For_Delegation_Datum <> valueOf_Delegated_MAYZ
                            ---------------------
                                !valueOf_Delegation_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_Delegation_Datum
                            in  valueOf_Delegation_Out `OnChainHelpers.isEqValue` valueFor_Delegation_Datum_Out_Control
                    ------------------
                T.PolicyRedeemerDeleteDelegationID _ ->
                    -- que se queme ID del Buy Order, 1 unidad. Creo que con esto es suficiente.
                    -- que se este ejecutando validador correcto. No seria necesario. Si se quema es por que sale de algun lado.
                    ---------------------
                        traceIfFalse "not isBurningDelegationID" isBurningDelegationID
                    ---------------------
                    where
                    ------------------
                        !valueFor_Burn_Delegation_ID = LedgerValue.assetClassValue delegation_ID_AC (negate 1)
                    ---------------------
                        isBurningDelegationID :: Bool
                        isBurningDelegationID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_Delegation_ID
                    -----------------
            then ()
            else error ()


--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Delegation_Datum_With_Deposit #-}
mkUpdated_Delegation_Datum_With_Deposit :: T.Delegation_DatumType -> Integer -> T.Delegation_DatumType
mkUpdated_Delegation_Datum_With_Deposit !delegation_Datum_In !delegated_Mayz_Change =
    T.mkDelegation_DatumType
        (T.ddDelegationPolicyID_CS delegation_Datum_In)
        (T.ddFundPolicy_CS delegation_Datum_In)
        (T.ddDelegatorPaymentPKH delegation_Datum_In)
        (T.ddDelegatorStakePKH delegation_Datum_In)
        (T.ddDelegated_Mayz delegation_Datum_In + delegated_Mayz_Change)
        (T.ddMinADA delegation_Datum_In)

--------------------------------------------------------------------------------2


{-# INLINEABLE mkValidator #-}
mkValidator :: T.CS ->  T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator tokenMAYZ_CS T.ValidatorParams {..} !datumRaw !redRaw !ctxRaw =
    if  traceIfFalse "" useThisToMakeScriptUnique
        && validateRedeemer getRedeemerType
            then ()
            else error ()
    where
    ------------------
        !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
        !input_Current_TxOut = OnChainHelpers.getUnsafe_Current_Input_TxOut ctx
    ------------------
        !delegation_Datum_In = T.getDelegation_DatumType datum
    ------------------
        !delegationPolicyID_CS = T.ddDelegationPolicyID_CS delegation_Datum_In
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- No hace falta uso las dos variables en la poliza
    ------------------
        redeemerDeposit = 1
        redeemerWithdraw = 2
        redeemerDelete = 3
    ------------------
        getRedeemerType :: Integer
        getRedeemerType = case redeemer of
            (T.ValidatorRedeemerDeposit _)  -> redeemerDeposit
            (T.ValidatorRedeemerWithdraw _) -> redeemerWithdraw
            (T.ValidatorRedeemerDelete _)   -> redeemerDelete
    ------------------
        validateRedeemer :: Integer -> Bool
        validateRedeemer redeemerType
            | redeemerType == redeemerDeposit = validateAdminAction (T.ddDelegatorPaymentPKH delegation_Datum_In) && validateDeposit redeemer
            | redeemerType == redeemerWithdraw = validateAdminAction (T.ddDelegatorPaymentPKH delegation_Datum_In) && validateWithdraw redeemer
            | redeemerType == redeemerDelete = validateAdminAction (T.ddDelegatorPaymentPKH delegation_Datum_In) && validateDelete
            | otherwise = False
    ------------------
        validateAdminAction :: T.WalletPaymentPKH -> Bool
        validateAdminAction !admin =
                traceIfFalse "not isSignedByAny admin" (OnChainHelpers.isSignedByAny [admin] info)
                && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
    ------------------
        validateDeposit :: T.ValidatorRedeemer  -> Bool
        validateDeposit (T.ValidatorRedeemerDeposit  T.ValidatorRedeemerDepositType{..})  =
        ---- add some MAYZ. Only delegator can do it.
        -- check that there is one input and one output in this contract: no estoy revisando si hay una sola entrada... hace falta ?
        -- check datum update with deposit
        -- check value changed with deposit
            traceIfFalse "not isCorrect_Output_Delegation_Datum_With_DelegationChanged" (isCorrect_Output_Delegation_Datum_With_DelegationChanged vrdDelegated_Mayz_Change)
            && traceIfFalse "not isCorrect_Output_Delegation_Value_With_DelegationChanged" (isCorrect_Output_Delegation_Value_With_DelegationChanged vrdDelegated_Mayz_Change)
        validateDeposit _   = False
    ------------------
        validateWithdraw :: T.ValidatorRedeemer  -> Bool
        validateWithdraw (T.ValidatorRedeemerWithdraw  T.ValidatorRedeemerWithdrawType{..})  =
        ---- get back some MAYZ. Only delegator can do it.
        -- check that there is one input and one output in this contract
        -- check datum update with withdraw ... me parece que no hay cambios que se hagan en el datum en esta tx
        -- check value changed with withdraw
            traceIfFalse "not isCorrect_Output_Delegation_Datum_With_DelegationChanged" (isCorrect_Output_Delegation_Datum_With_DelegationChanged vrdwDelegated_Mayz_Change)
            && traceIfFalse "not isCorrect_Output_Delegation_Value_With_DelegationChanged" (isCorrect_Output_Delegation_Value_With_DelegationChanged vrdwDelegated_Mayz_Change)
        validateWithdraw _   = False
    ------------------
        validateDelete :: Bool
        validateDelete = traceIfFalse "not isBurningDelegationID" isBurningDelegationID
        ---- get back all MAYZ and delete datum utxo. Burn delegation ID. only delegator can do it
        -- check that there is one input and zero output in this contract
        -- check that ID is burning
    ------------------
        getOutput_Own_TxOut_And_Delegation_Datum :: (LedgerApiV2.TxOut, T.Delegation_DatumType)
        getOutput_Own_TxOut_And_Delegation_Datum =
            let
                !outputs_Own_TxOuts = OnChainHelpers.getUnsafe_Own_Outputs_TxOuts ctx
            ------------------
                !outputs_Own_TxOuts_And_Delegation_Datums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.Delegation_DatumType outputs_Own_TxOuts ctx delegationPolicyID_CS T.getDelegation_DatumType
                !output_Own_TxOut_And_Delegation_Datum = case outputs_Own_TxOuts_And_Delegation_Datums of
                    [x] -> x
                    _   -> traceError "expected exactly one Delegation output"
            ------------------
            in output_Own_TxOut_And_Delegation_Datum
    ------------------
        isCorrect_Output_Delegation_Datum_With_DelegationChanged:: Integer -> Bool
        isCorrect_Output_Delegation_Datum_With_DelegationChanged delegated_Mayz_Change =
            let !output_Own_TxOut_And_Delegation_Datum = getOutput_Own_TxOut_And_Delegation_Datum
            ------------------
                !delegation_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_Delegation_Datum
                !delegation_Datum_Out_Control = mkUpdated_Delegation_Datum_With_Deposit delegation_Datum_In delegated_Mayz_Change
            in  delegation_Datum_Out `OnChainHelpers.isUnsafeEqDatums` delegation_Datum_Out_Control
    ------------------
        isCorrect_Output_Delegation_Value_With_DelegationChanged :: Integer -> Bool
        isCorrect_Output_Delegation_Value_With_DelegationChanged delegated_Mayz_Change =
            let !output_Own_TxOut_And_Delegation_Datum = getOutput_Own_TxOut_And_Delegation_Datum
            ------------------
                !valueOf_Delegation_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_Delegation_Datum
            ------------------
                !tokenMAYZ_AC = LedgerValue.AssetClass (tokenMAYZ_CS, T.tokenMAYZ_TN)
                !valueOf_Delegated_MAYZ = LedgerValue.assetClassValue tokenMAYZ_AC delegated_Mayz_Change
                !valueFor_Delegation_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> valueOf_Delegated_MAYZ
            in  valueOf_Delegation_Out `OnChainHelpers.isEqValue` valueFor_Delegation_Out_Control
    ------------------
        isBurningDelegationID :: Bool
        isBurningDelegationID  =
            let !delegationID_AC = LedgerValue.AssetClass (delegationPolicyID_CS, T.delegationID_TN)
            in OnChainHelpers.isNFT_Burning_With_AC delegationID_AC info

----------------------------------------------------------------------------

{-# INLINEABLE policyID #-}
policyID ::  T.CS ->  T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID tokenMAYZ_CS params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode T.tokenMAYZ_CS
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE validator #-}
validator :: T.CS ->  T.ValidatorParams -> LedgerApiV2.Validator
validator tokenMAYZ_CS params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode T.tokenMAYZ_CS
                    `PlutusTx.applyCode` PlutusTx.liftCode params

------------------------------------------------------------------------------
