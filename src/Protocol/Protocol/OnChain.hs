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
module Protocol.Protocol.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

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
import qualified Generic.OnChainHelpers    as Helpers
import qualified Generic.OnChainHelpers    as OnChainHelpers
import qualified Generic.Types             as T
import qualified Protocol.Constants        as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.Protocol.Helpers as Helpers
import qualified Protocol.Protocol.Types   as T
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2



{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID T.PolicyParams {..} _ !ctxRaw =
    -- Que se consuma utxo en parámetro de la póliza
    -- Que se este minteando NFT con el nombre correcto
    let !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
        !protocolPolicyID_TxOutRef = ppProtocolPolicyID_TxOutRef
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- no hace falta, el unico parametro esta siendo usado en la póliza
    ------------------
        !protocolPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
    ------------------
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
    ------------------
        !valueFor_Mint_ProtocolID = LedgerValue.assetClassValue protocolID_AC 1
    ------------------
        isMintingID :: Bool
        isMintingID = Helpers.getUnsafeOwnMintingValue ctx `Helpers.isEqValue` valueFor_Mint_ProtocolID
    -----------------
    in  if  traceIfFalse "" useThisToMakeScriptUnique
            && traceIfFalse "not isTxOutAnInput" (Helpers.isTxOutAnInput protocolPolicyID_TxOutRef info)
            && traceIfFalse "not isMintingID" isMintingID
        then ()
        else error ()

--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator T.ValidatorParams {..} !datumRaw !redRaw !ctxRaw =
    if
        traceIfFalse "" useThisToMakeScriptUnique
        && validateAdminAction (T.pdAdmins protocolDatum_In)
        && validateRedeemerProtocolAdmin
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
            !useThisToMakeScriptUnique = True -- no hace falta, el unico parametro esta siendo usado en la póliza
        ------------------
            !input_Current_TxOut = Helpers.getUnsafe_Current_Input_TxOut ctx
        ------------------
            !protocolDatum_In = T.getProtocolDatumType datum
        ------------------
            !valueOf_ProtocolDatum_In = LedgerApiV2.txOutValue input_Current_TxOut
        ------------------
            !outputs_Own_TxOuts = Helpers.getUnsafe_Own_Outputs_TxOuts ctx
        ------------------
            !outputs_Own_TxOuts_And_ProtocolDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.ProtocolDatumType outputs_Own_TxOuts ctx protocolPolicyID_CS T.getProtocolDatumType
            !output_Own_TxOut_And_ProtocolDatum = case outputs_Own_TxOuts_And_ProtocolDatums of
                [x] -> x
                _   -> traceError "expected exactly one Protocol output"
        ------------------
            validateAdminAction :: [T.WalletPaymentPKH] -> Bool
            validateAdminAction !admins =
                    -- Que sea Protocol Admin
                       traceIfFalse "not isSignedByAny admins" (Helpers.isSignedByAny admins info)
                    && traceIfFalse "not isValidRange" (Helpers.isValidRange info T.validTimeRange) -- TODO: es necesario?
        ------------------
            validateRedeemerProtocolAdmin :: Bool
            validateRedeemerProtocolAdmin = case redeemer of
                (T.ValidatorRedeemerDatumUpdate redeemerType) ->
                ------------------
                    -- Que el ProtocolDatum regrese a Protocol Val
                    -- Que el ProtocolDatum se actualiza correctamente
                    -- Que el ProtocolDatum value no cambie
                ------------------
                       traceIfFalse "not isCorrect_Output_ProtocolDatum_Updated" (isCorrect_Output_ProtocolDatum_Updated redeemerType)
                    && traceIfFalse "not isCorrect_Output_ProtocolDatum_Value_NotChanged" isCorrect_Output_ProtocolDatum_Value_NotChanged
        ------------------
            isCorrect_Output_ProtocolDatum_Updated :: T.ValidatorRedeemerDatumUpdateType -> Bool
            isCorrect_Output_ProtocolDatum_Updated T.ValidatorRedeemerDatumUpdateType  =
                let !protocolDatum_Out = Helpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_ProtocolDatum
                    !protocolDatum_Out_Control = Helpers.mkUpdated_ProtocolDatum_With_NormalChanges protocolDatum_In
                            (T.pdOraclePaymentPubKey protocolDatum_Out)
                            (T.pdAdmins protocolDatum_Out)
                            (T.pdFundClasses protocolDatum_Out)
                            (T.pdFundLifeTime protocolDatum_Out)
                            (T.pdRequiredMAYZForSellOffers protocolDatum_Out)
                            (T.pdRequiredMAYZForBuyOrders protocolDatum_Out)
                            (T.pdCommissionFunds protocolDatum_Out)
                            (T.pdCommissionSellOffers protocolDatum_Out)
                            (T.pdCommissionBuyOrders protocolDatum_Out)
                            (T.pdShare_Protocol protocolDatum_Out)
                            (T.pdShare_MAYZ protocolDatum_Out)
                            (T.pdShare_FundAdmins protocolDatum_Out)
                            (T.pdMAYZWallets protocolDatum_Out)
                in  protocolDatum_Out `Helpers.isUnsafeEqDatums` protocolDatum_Out_Control
        ------------------
            isCorrect_Output_ProtocolDatum_Value_NotChanged :: Bool
            isCorrect_Output_ProtocolDatum_Value_NotChanged =
                let !valueOf_ProtocolDatum_Out = Helpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_ProtocolDatum
                    !valueFor_ProtocolDatum_Control = valueOf_ProtocolDatum_In
                in  valueOf_ProtocolDatum_Out `Helpers.isEqValue` valueFor_ProtocolDatum_Control

--------------------------------------------------------------------------------2

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

--------------------------------------------------------------------------------2
