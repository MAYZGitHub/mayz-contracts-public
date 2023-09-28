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
module Protocol.Script.OnChain where

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

import qualified Generic.OnChainHelpers    as Helpers
import qualified Generic.Types             as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.Script.Types     as T
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID  T.PolicyParams{..} !redRaw !ctxRaw =
    let !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
    ------------------
        !protocolPolicyID_CS = ppProtocolPolicyID_CS
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
    ------------------
    in  if traceIfFalse "" useThisToMakeScriptUnique
            &&
            case redeemer of
            T.PolicyRedeemerBurnID _ ->
                traceIfFalse "not isBurningAllTokenOwnCSAnyAmount" (Helpers.isBurningAllTokenOwnCSAnyAmount ctx)
            T.PolicyRedeemerMintID _ ->
                traceIfFalse "not isCorrectMint_And_Outputs" isCorrectMint_And_Outputs
                where
                    isCorrectMint_And_Outputs :: Bool
                    !isCorrectMint_And_Outputs =
                        let !flattenValueOf_Script_IDs = Helpers.getUnsafeOwnMintingTokenNameAndAmt ctx
                            !cs = LedgerContextsV2.ownCurrencySymbol ctx
                            ------------------
                            -- TODO: esta el campo de scxript hash en el datum que no lo estoy controlando
                            ------------------
                            !outputs_TxOuts_And_ScriptDatum = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_CS @T.ValidatorDatum @T.ScriptDatumType ctx cs T.getScriptDatumType
                            ------------------
                            isCorrectAmt_And_TN :: (LedgerApiV2.TokenName, Integer) -> (LedgerContextsV2.TxOut, d) -> Bool
                            isCorrectAmt_And_TN (!tn, !amt) (!txOut, _) = amt == 1 && isCorrectTN tn txOut
                            ------------------
                            isCorrectTN :: LedgerApiV2.TokenName -> LedgerApiV2.TxOut -> Bool
                            isCorrectTN tn txOut = LedgerApiV2.getScriptHash (Helpers.fromJust $ LedgerApiV2.txOutReferenceScript txOut) == LedgerApiV2.unTokenName tn
                        in  Helpers.compareWithFunctionWhileRemoving flattenValueOf_Script_IDs outputs_TxOuts_And_ScriptDatum isCorrectAmt_And_TN
            then ()
            else error ()


--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator T.ValidatorParams {..} _ !redRaw !ctxRaw =
    let !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
    ------------------
        !protocolPolicyID_CS = vpProtocolPolicyID_CS
        !scriptPolicyID_CS = vpScriptPolicyID_CS
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- No hace falta uso las dos variables en la poliza
    ------------------
    in  if
        traceIfFalse "" useThisToMakeScriptUnique
        && case redeemer of
            T.ValidatorRedeemerScriptDelete ->
                       traceIfFalse "not isSignedByAdmin" isSignedByAdmin
                    && traceIfFalse "not isBurning_ScriptIDs" isBurning_ScriptIDs
                    && traceIfFalse "not isCorrectAmount_SendBackToAdmin" isCorrectAmount_SendBackToAdmin
                where
                    !inputs_Own_TxOuts = Helpers.getUnsafe_Own_Inputs_TxOuts ctx
                    !inputs_Own_TxOuts_And_ScriptDatums = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.ScriptDatumType inputs_Own_TxOuts ctx scriptPolicyID_CS T.getScriptDatumType
                    ------------------
                    !scriptDatums_In = Helpers.getDatum_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_ScriptDatums
                    ------------------
                    isSignedByAdmin :: Bool
                    !isSignedByAdmin =
                        let !inputsRef_TxOuts_And_ProtocolDatum = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_CS @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType ctx protocolPolicyID_CS ProtocolT.getProtocolDatumType
                        ------------------
                            !isSignedEach =
                                [ let  !admins = case T.sdFundPolicy_CS scriptDatum_In of
                                            Nothing -> case inputsRef_TxOuts_And_ProtocolDatum of
                                                            [inputRef_TxOut_And_ProtocolDatum] -> (T.getAdmins . Helpers.getDatum_In_TxOut_And_Datum) inputRef_TxOut_And_ProtocolDatum
                                                            _                                  -> traceError "expected exactly Protocol input ref"
                                            Just cs ->
                                                let !inputsRef_TxOuts_And_FundDatum = Helpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_CS @FundT.ValidatorDatum @FundT.FundDatumType ctx cs FundT.getFundDatumType
                                                in case inputsRef_TxOuts_And_FundDatum of
                                                        [inputRef_TxOuts_And_FundDatum] -> (T.getAdmins . Helpers.getDatum_In_TxOut_And_Datum) inputRef_TxOuts_And_FundDatum
                                                        _                               -> traceError "expected exactly one Fund input ref"
                                    in Helpers.isSignedByAny admins info
                                  | scriptDatum_In <- scriptDatums_In
                                ]
                        in  all (== True) isSignedEach
                    ------------------
                    isBurning_ScriptIDs :: Bool
                    !isBurning_ScriptIDs =
                        let !valueOf_Inputs_Own_TxOuts_And_ScriptDatums = mconcat $ Helpers.getValue_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_ScriptDatums
                        ------------------
                            !flattenValueOf_Script_IDs = Helpers.flattenValue $ LedgerValue.noAdaValue valueOf_Inputs_Own_TxOuts_And_ScriptDatums
                        in  all
                                ( \(cs, tn, amt) ->
                                    let scriptID_AC = LedgerValue.AssetClass (cs, tn)
                                    in  Helpers.isToken_Minting_With_AC_AndAmt scriptID_AC (negate amt) info
                                )
                                flattenValueOf_Script_IDs
                    ------------------
                    isCorrectAmount_SendBackToAdmin :: Bool
                    !isCorrectAmount_SendBackToAdmin =
                        let joinSameAdmin :: [(T.WalletPaymentPKH, LedgerApiV2.Value)] -> [(T.WalletPaymentPKH, LedgerApiV2.Value, LedgerApiV2.Value)]
                            joinSameAdmin = joinSameAdminHelper []
                                where
                                    joinSameAdminHelper :: [(T.WalletPaymentPKH, LedgerApiV2.Value, LedgerApiV2.Value)] -> [(T.WalletPaymentPKH, LedgerApiV2.Value)] -> [(T.WalletPaymentPKH, LedgerApiV2.Value, LedgerApiV2.Value)]
                                    joinSameAdminHelper seen [] = seen
                                    joinSameAdminHelper seen ((admin_To_SendBack, valueOf_ScriptDatum) : xs) =
                                        let !admin' = Helpers.find' (\(m, _, _) -> m == admin_To_SendBack) seen
                                        in  case admin' of
                                                Nothing ->
                                                    let !valueFor_Admin_Real = LedgerContextsV2.valuePaidTo info admin_To_SendBack
                                                        !elemet = (admin_To_SendBack, valueOf_ScriptDatum, valueFor_Admin_Real)
                                                    in  joinSameAdminHelper (elemet : seen) xs
                                                Just (_, v1, v2) ->
                                                    let !elemet = (admin_To_SendBack, v1 <> valueOf_ScriptDatum, v2)
                                                        !seen_filter = Helpers.filter' (\(m', _, _) -> m' /= admin_To_SendBack) seen
                                                    in  joinSameAdminHelper (elemet : seen_filter) xs
                        ------------------
                            !values_For_Each_Admin =
                                [ let !scriptDatum_In = Helpers.getDatum_In_TxOut_And_Datum input_Own_TxOut_And_ScriptDatum
                                      !admin_To_SendBack = T.sdAdminPaymentPKH scriptDatum_In
                                      !valueFor_Admin = LedgerValue.adaOnlyValue $ Helpers.getValue_In_TxOut_And_Datum input_Own_TxOut_And_ScriptDatum
                                  in  (admin_To_SendBack, valueFor_Admin)
                                  | input_Own_TxOut_And_ScriptDatum <- inputs_Own_TxOuts_And_ScriptDatums
                                ]
                        ------------------
                            !values_For_Each_Admin_Accumulated = joinSameAdmin values_For_Each_Admin
                        in  all (\(_, v1, v2) -> Helpers.isIncludeValue' v2 v1) values_For_Each_Admin_Accumulated
            then ()
            else error ()

----------------------------------------------------------------------------

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

------------------------------------------------------------------------------
