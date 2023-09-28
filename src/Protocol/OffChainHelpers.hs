{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}

--------------------------------------------------------------------------------2

module Protocol.OffChainHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad                       as Monad
import qualified Data.Aeson                          as DataAeson
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid
import qualified Ledger
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.TxConstraints    as LedgerTxConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Tx                           as LedgerTx
import qualified Ledger.Typed.Scripts                as UtilsTypedScriptsValidatorsV1
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.Constants                   as T
import qualified Generic.OffChainEval                as OffChainEval
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Generic.Types                       as T
import qualified Protocol.Constants                  as T
import qualified Protocol.Fund.Helpers               as Helpers
import qualified Protocol.Fund.Holding.Types         as FundHoldingT
import qualified Protocol.Fund.Types                 as FundT
import qualified Protocol.InvestUnit.Types           as InvestUnitT
import qualified Protocol.PABTypes                   as T
import qualified Protocol.Protocol.Types             as ProtocolT
import qualified Protocol.Script.Types               as ScriptT
import qualified Protocol.SellOffer.Types            as SellOfferT
import qualified Protocol.Types                      as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

getFullUTxO_With_ProtocolDatum_By_AC :: LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, ProtocolT.ProtocolDatumType)
getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator = do
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType protocolID_AC uTxOsAt_ProtocolValidator ProtocolT.getProtocolDatumType
    case utxos of
        [] -> do
            PlutusContract.throwError "No uTxO_With_ProtocolDatum found"
        _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxO_With_ProtocolDatum found: %s" (P.show $ (\(ref, _, _) -> ref) (head utxos))
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return $ head utxos

--------------------------------------------------------------------------------2

getFullUTxO_With_FundDatum_By_AC :: LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, FundT.FundDatumType)
getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator = do
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @FundT.ValidatorDatum @FundT.FundDatumType fundID_AC uTxOsAt_FundValidator FundT.getFundDatumType
    case utxos of
        [] -> do
            PlutusContract.throwError "No uTxO_With_FundDatum found"
        _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxO_With_FundDatum found: %s" (P.show $ (\(ref, _, _) -> ref) (head utxos))
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return $ head utxos

--------------------------------------------------------------------------------2

getFullUTxO_With_InvestUnitDatum_By_AC :: LedgerValue.AssetClass -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, InvestUnitT.InvestUnitDatumType)
getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator = do
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @InvestUnitT.ValidatorDatum @InvestUnitT.InvestUnitDatumType investUnitID_AC uTxOsAt_InvestUnitValidator InvestUnitT.getInvestUnitDatumType
    case utxos of
        [] -> do
            PlutusContract.throwError "No uTxO_With_InvestUnitDatum found"
        _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxO_With_InvestUnitDatum found: %s" (P.show $ (\(ref, _, _) -> ref) (head utxos))
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return $ head utxos

--------------------------------------------------------------------------------2

getFullUTxO_With_FundHoldingDatum_By_CS :: LedgerApiV2.CurrencySymbol -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, FundHoldingT.FundHoldingDatumType)
getFullUTxO_With_FundHoldingDatum_By_CS fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator = do
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator FundHoldingT.getFundHoldingDatumType
    case utxos of
        [] -> do
            PlutusContract.throwError "No uTxO_With_FundHoldingDatum found"
        _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxO_With_FundHoldingDatum found: %s" (P.show $ (\(ref, _, _) -> ref) (head utxos))
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return $ head utxos

--------------------------------------------------------------------------------2

getFullUTxO_With_FundHoldingDatum_And_Enough_Subtotal_By_CS :: Integer -> LedgerApiV2.CurrencySymbol -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, FundHoldingT.FundHoldingDatumType)
getFullUTxO_With_FundHoldingDatum_And_Enough_Subtotal_By_CS amount fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator = do
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator FundHoldingT.getFundHoldingDatumType
    case utxos of
        [] -> do
            PlutusContract.throwError "No uTxO_With_FundHoldingDatum found"
        _ -> do
            let selected = [ utxo | utxo <- utxos, (\(_, _, datum) -> FundHoldingT.hdSubtotal_FT_Minted datum >= amount) utxo]
            case selected of
                [] -> do
                    PlutusContract.throwError "No uTxO_With_FundHoldingDatum with enough amount"
                _ -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxO_With_FundHoldingDatum found: %s" (P.show $ (\(ref, _, _) -> ref) (head selected))
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
                    return $ head selected

--------------------------------------------------------------------------------2

getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS :: LedgerApiV2.TxOutRef -> LedgerApiV2.CurrencySymbol -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, FundHoldingT.FundHoldingDatumType)
getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator = do
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator FundHoldingT.getFundHoldingDatumType
    case utxos of
        [] -> do
            PlutusContract.throwError "No uTxO_With_FundHoldingDatum found"
        _ -> do
            let selected = [ utxo | utxo <- utxos, (\(ref, _, __) -> ref == holdingTxOutRef) utxo]
            case selected of
                [] -> do
                    PlutusContract.throwError "No uTxO_With_FundHoldingDatum with enough amount"
                _ -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxO_With_FundHoldingDatum found: %s" (P.show $ (\(ref, _, _) -> ref) (head selected))
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
                    return $ head selected

--------------------------------------------------------------------------------2

getMaybeUTxO_With_ScriptRef :: P.String -> LedgerApiV2.ScriptHash -> LedgerApiV2.CurrencySymbol -> DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut))
getMaybeUTxO_With_ScriptRef name scriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator = do
    let !scriptID_TN = LedgerApiV2.TokenName $ LedgerApiV2.getScriptHash scriptHash
        !scriptID_AC = LedgerValue.AssetClass (scriptPolicyID_CS, scriptID_TN)
    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @ScriptT.ValidatorDatum @ScriptT.ScriptDatumType scriptID_AC uTxOsAt_ScriptValidator ScriptT.getScriptDatumType
    case utxos of
        [] -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "No %s (%s) to use as ScriptRef found" name (P.show scriptHash)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return Nothing
        _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "%s (%s) to use as ScriptRef at %s" name (P.show scriptHash) (P.show $ (\(ref, _, _) -> ref) (head utxos))
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return $ Just $ (\ (r, de, _) -> (r, de)) (head utxos)

--------------------------------------------------------------------------------2

mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' :: (PlutusTx.ToData redeemer) =>
    (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut, datum) ->
    redeemer ->
    Maybe (LedgerApiV2.TxOutRef, LedgerTx.DecoratedTxOut) ->
    Maybe LedgerApiV2.Validator ->
    (LedgerConstraints.ScriptLookups a0, LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void))
mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_To_Consume =
    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy ((\(ref, dec, _) -> (ref, dec)) uTxO_To_Consume)


--------------------------------------------------------------------------------2

evalAndSubmitTx' ::
    forall w s.
    P.String -> T.ProtocolPABParams -> Maybe T.FundPABParams ->
    LedgerConstraints.ScriptLookups DataVoid.Void ->
    LedgerTxConstraints.TxConstraints (UtilsTypedScriptsValidatorsV1.RedeemerType DataVoid.Void) (UtilsTypedScriptsValidatorsV1.DatumType DataVoid.Void) ->
    PlutusContract.Contract w s DataText.Text ()
evalAndSubmitTx' nameEndPoint protocolPABParams fundPABParams' lookupsTx tx = do
    let
        protocolPolicyID = T.pppProtocolPolicyID protocolPABParams
        protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        protocolValidator = T.pppProtocolValidator protocolPABParams
        protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
    ---------------------
        scriptPolicyID = T.pppScriptPolicyID protocolPABParams
        scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        scriptValidator = T.pppScriptValidator protocolPABParams
        scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
    ---------------------
    let !eval_MintingPolicies' = [
            (protocolPolicyID_CS, protocolPolicyID),
            (scriptPolicyID_CS, scriptPolicyID)]
        !eval_Validators' = [
            (protocolValidator_Address, protocolValidator),
            (scriptValidator_Address, scriptValidator)
            ]
        !eval_GetDatums = [
            T.showCborAsDatumType @ProtocolT.ValidatorDatum,
            T.showCborAsDatumType @FundT.ValidatorDatum,
            T.showCborAsDatumType @InvestUnitT.ValidatorDatum,
            T.showCborAsDatumType @FundHoldingT.ValidatorDatum,
            T.showCborAsDatumType @ScriptT.ValidatorDatum
         ]
    ---------------------
    let (!eval_MintingPolicies, !eval_Validators) = case fundPABParams' of
            Nothing -> (eval_MintingPolicies', eval_Validators')
            Just fundPABParams ->
                let
                    fundPolicy = T.fppFundPolicy fundPABParams
                    fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
                    fundValidator = T.fppFundValidator fundPABParams
                    fundValidator_Address = T.fppFundValidator_Address fundPABParams
                    fundHoldingPolicyID = T.fppFundHoldingPolicyID fundPABParams
                    fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
                    fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
                    fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
                    investUnitValidator = T.fppInvestUnitValidator fundPABParams
                    investUnitValidator_Address = T.fppInvestUnitValidator_Address fundPABParams
                ---------------------
                    !eval_MintingPolicies'' = [
                        (fundPolicy_CS, fundPolicy),
                        (fundHoldingPolicyID_CS, fundHoldingPolicyID)]
                    !eval_Validators'' = [
                        (fundValidator_Address, fundValidator),
                        (fundHoldingValidator_Address, fundHoldingValidator),
                        (investUnitValidator_Address, investUnitValidator)
                        ]
                in (eval_MintingPolicies'' ++ eval_MintingPolicies', eval_Validators'' ++ eval_Validators')
    ---------------------
    OffChainEval.evalAndSubmitTx nameEndPoint eval_MintingPolicies eval_Validators eval_GetDatums lookupsTx tx

--------------------------------------------------------------------------------2

checkCollateral :: DataMap.Map LedgerApiV2.TxOutRef LedgerTx.DecoratedTxOut  -> PlutusContract.Contract w s DataText.Text ()
checkCollateral uTxOsAtUser = do
    !swCheckCollateral <- OffChainHelpers.checkIfThereIsUTxOFreeForCollateral uTxOsAtUser
    if swCheckCollateral
        then return ()
        else PlutusContract.throwError "There is NOT UTxO free for Collateral"

--------------------------------------------------------------------------------

-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f030405ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff0000008000ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f040506ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff0000009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff00ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100008080d8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"

-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f019fd8799f01581ce1492c3e12463b0039b36513ce5f50e391cb6b7773905897f9ff74f3581c4371d5ba8cbd3e330f2417d2d04cf44b258d9cff207e90188c91c291581c3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712ffff581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f19115c1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f00001901bcffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f1b0000040ad427de4a1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f00001901bcffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f1b0000040ad427de4a1a004c4b401a05f5e100ffd8799f1915b31a004c4b401a05f5e100ffffd8799f000000ff0000d8799f00001901bcffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"

-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ca44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581cf44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f011a004c4b401a05f5e100ffd8799f021a004c4b401a05f5e100ffd8799f19386b1a004c4b401a05f5e100ffd8799f1a2067346f1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"

readStringDecodedAsProtocolValidatorDatum :: P.String -> P.IO ProtocolT.ValidatorDatum
readStringDecodedAsProtocolValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ProtocolT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d8799fd8799fd8799fd8799f581c47d39eec62a0069c145d691757d2dec20d879a949e0f603b9f3035a346506f6f6c4944ff9f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16eff1b000001835d3453f09fd8799f185a01ffd8799f18b402ffd8799f19270f03ffff1a02faf080581c14f792644b7ce8e294ef44826e6017aee704f9d5f2bfa8b83ce9d38effd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f582025d3b9007a5ab81ef517aea2448dccddd6b494d8404ff000cd23c508e8762c79ff00ffffffffff\"}"
-- readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d87980\"}"
-- readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d87a9fd8799f01ffff\"}"



readStringDecodedAsProtocolValidatorRedeemer :: P.String -> P.IO ProtocolT.ValidatorRedeemer
readStringDecodedAsProtocolValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ProtocolT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result



--------------------------------------------------------------------------------

-- readStringDecodedAsScriptValidatorDatum "{\"getDatum\":\"d8799fd8799fd87a80581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabd87a80581c3a996e7ae6bb0444d0fddad841e0b843c6fb819788853670dd9a417affff\"}"

readStringDecodedAsScriptValidatorDatum :: P.String -> P.IO ScriptT.ValidatorDatum
readStringDecodedAsScriptValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ScriptT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsPolicyRedeemerRedeemer "{\"getRedeemer\":\"d87980\"}"

readStringDecodedAsPolicyRedeemerRedeemer :: P.String -> P.IO ScriptT.PolicyRedeemer
readStringDecodedAsPolicyRedeemerRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ScriptT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


--------------------------------------------------------------------------------

-- readStringDecodedAsFundValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c31bfa53cf3cd05ddb7e3e2230d78bbdbd6735c4018a1deb6b022a60c581c71818ecb72f7f5ac542566360ae318a3dd25a2541db9791414b09a2e581cf1e013aee32c3c7b5e318d8e24e50a4e1e1a03430c0a2212096c185e9f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b0000018971a313b81b00000189ad3dddb8d87a8000000080001a001ef015ffff\"}"
-- readStringDecodedAsFundValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c9eca19af35e1e13892a7ea1215a163b32fa68db748863521c067e26b581c7287baa38b03be764476101673c803b4d6d85640e37addce20ad2d68581cb1cb50589461b2f66955ca45707eb80a7a5a289600953ec5ef96843a9f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001897ab39aa31b00000189b64e64a3d87a8000000080001a001ef015ffff\"}"
-- readStringDecodedAsFundValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c7a1cea9e91d69cee0aee8cb3a61e3cf1c815ce9b1793c83d212d1614581c7ab790f20e31972f1b1e84c0e7468932018cdae8010aac890ed9a65f581c1a5595a496b3cc8161a61bff8109bdab3ddb30f1ff3d9ddb07ffda069f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001897ae5973d1b00000189b680613dd87a8000000080001a001ef015ffff\"}"
-- readStringDecodedAsFundValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c7a1cea9e91d69cee0aee8cb3a61e3cf1c815ce9b1793c83d212d1614581c7ab790f20e31972f1b1e84c0e7468932018cdae8010aac890ed9a65f581c1a5595a496b3cc8161a61bff8109bdab3ddb30f1ff3d9ddb07ffda069f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001897af078151b00000189b68b4216d87a8000000080001a001f76c7ffff\"}"

-- readStringDecodedAsFundValidatorDatum "{\"getDatum\":\"d8799fd8799f02581cb7eb625c4289e5c6caacd4c631d7290f83d3d7fc8032d3ddc287a87b581c0fee00f9d38692d31c707a74fc4296c6d81ddd3853981c4c93115a43581c113a210c5942a82f65bfb879b91a728183966419959afef6a59b7e23581c047d3f4f14e687440923f6f20cf16f9e83b8887f64c3073268429d6c581c5458e7503d967d7b19a5b595552b5d4fcb1e9681e332b9d9a40688949f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001898581977a1b00000189c11c617ad87a8000000080001a00218417ffff\"}"


readStringDecodedAsFundValidatorDatum :: P.String -> P.IO FundT.ValidatorDatum
readStringDecodedAsFundValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @FundT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


-- readStringDecodedAsFundPolicyRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"



readStringDecodedAsFundPolicyRedeemer :: P.String -> P.IO FundT.PolicyRedeemer
readStringDecodedAsFundPolicyRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @FundT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


-- readStringDecodedAsFundValidatorRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"

readStringDecodedAsFundValidatorRedeemer :: P.String -> P.IO FundT.ValidatorRedeemer
readStringDecodedAsFundValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @FundT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


--------------------------------------------------------------------------------
-- readStringDecodedAsInvestUnitValidatorDatum "{\"getDatum\":\"d8799fd8799f581c9eca19af35e1e13892a7ea1215a163b32fa68db748863521c067e26bd8799fd8799f41aa41aa05ffd8799f41aa410005ffff1a001ef015ffff\"}"

-- readStringDecodedAsInvestUnitValidatorDatum "{\"getDatum\":\"d8799fd8799f581c9eca19af35e1e13892a7ea1215a163b32fa68db748863521c067e26bd8799f9f9f404005ffffff1a001ef015ffff\"}"
-- readStringDecodedAsInvestUnitValidatorDatum "{\"getDatum\":\"d8799fd8799f581c9eca19af35e1e13892a7ea1215a163b32fa68db748863521c067e26bd8799f9fd8799f41aa41aa05ffd8799f41bb41bb1835ffffff1a001ef015ffff\"}"
-- readStringDecodedAsInvestUnitValidatorDatum "{\"getDatum\":\"d8799fd8799f581c7a1cea9e91d69cee0aee8cb3a61e3cf1c815ce9b1793c83d212d1614d8799f9fd8799f41aa41aa05ffd8799f41aa41bb04ffffff1a001ec7adffff\"}"
-- readStringDecodedAsInvestUnitValidatorDatum "{\"getDatum\":\"d8799fd8799f581cb7eb625c4289e5c6caacd4c631d7290f83d3d7fc8032d3ddc287a87bd8799f9fd8799f404005ffffff1a00218417ffff\"}"



readStringDecodedAsInvestUnitValidatorDatum :: P.String -> P.IO InvestUnitT.ValidatorDatum
readStringDecodedAsInvestUnitValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @InvestUnitT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

--------------------------------------------------------------------------------

-- readStringDecodedAsInvestUnitValidatorRedeemer "{\"getRedeemer\":\"d8799fd8799fd8799f9fd8799f404009ffffffd8799f9fd8799f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab4651574552545901ffffffd8799fd8799f9fd8799f404001ffd8799f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab4651574552545901ffffff1b00000189f68ee241ff5f5840845846a201276761646472657373583900abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e026c6fbd39eae18dabbe11021b5b9901635e0158405bf3df6f83798fde09a166686173686564f440584054180005366f5beb28db240c3d7c3741971eacbd02a2c1bd768792d271bab8ad323966d130fa405bd7d4d85584b4e0eb11eea2fe107f6b884763d4ca50c33b5d0effffff\"}"
-- readStringDecodedAsInvestUnitValidatorRedeemer "{\"getRedeemer\":\"d8799fd8799fd8799f9fd8799f404009ffffffd8799f9fd8799f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab4651574552545901ffffffd8799fd8799f9fd8799f404001ffd8799f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab4651574552545900ffffff1b00000189f69068e1ff58403ab11c43a69d078ab0c25bfa0f518a17c0cc40b620a0c5bbacc65b734b6a46ea2b506f06be1a9d2207f53469fe6f710e3ca04b2921f6fa4f99de1271b6c87009ffff\"}"



readStringDecodedAsInvestUnitValidatorRedeemer :: P.String -> P.IO InvestUnitT.ValidatorRedeemer
readStringDecodedAsInvestUnitValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @InvestUnitT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


--------------------------------------------------------------------------------

-- readStringDecodedAsFundHoldingValidatorDatum "{\"getDatum\":\"d8799fd8799f000000000000000000001a00223332ffff\"}"
-- readStringDecodedAsFundHoldingValidatorDatum "{\"getDatum\":\"d8799fd8799f000a0a0a0000000000001a00223332ffff\"}"
-- readStringDecodedAsFundHoldingValidatorDatum "{\"getDatum\":\"d87e9fd8799fd8799f9fd8799f404009ffffffd8799f9fd8799f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab4651574552545901ffffffffff\"}"
-- readStringDecodedAsFundHoldingValidatorDatum "{\"getDatum\":\"d87e9fd8799fd8799f9fd8799f404009ffffffd8799f9fd8799f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab4651574552545901ffffffffff\"}"


readStringDecodedAsFundHoldingValidatorDatum :: P.String -> P.IO FundHoldingT.ValidatorDatum
readStringDecodedAsFundHoldingValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @FundHoldingT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsFundHoldingPolicyRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"

readStringDecodedAsFundHoldingPolicyRedeemer :: P.String -> P.IO FundHoldingT.PolicyRedeemer
readStringDecodedAsFundHoldingPolicyRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @FundHoldingT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

--------------------------------------------------------------------------------

-- readStringDecodedAsSellOfferValidatorRedeemer "{\"getRedeemer\":\"d87e9fd8799f0a19021705d8799fd8799f9fd8799f581c70f7fee602900e395d84c1a3cfe1603303fb8c48435777be6455a8bc44343635341836ffffff1b0000018a52a7ad15ff5840529e9eb87717a6fbe10203615a465e50eb99ae2908e6ab0dc94302ec18f4b787b24aefce16b772795dcfde40947037952fe91888919ed06049c2362ed1e19a0dffff\"}"


readStringDecodedAsSellOfferValidatorRedeemer :: P.String -> P.IO SellOfferT.ValidatorRedeemer
readStringDecodedAsSellOfferValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @SellOfferT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


-- readStringDecodedAsSellOfferValidatorDatum "{\"getDatum\":\"d8799fd8799f000000000000000000001a00223332ffff\"}"
-- readStringDecodedAsSellOfferValidatorDatum "{\"getDatum\":\"d87e9fd8799f0a19022105d8799fd8799f80ff1b0000018a52965795ff58406b8d97c54e097f202c3f2a443242c9db22164414a0c149147035475c85d4d3443dd941151f6a02b5375331cc21621f554753e896ddfcd2d8f05f735d62b7d90cffff\"}"


readStringDecodedAsSellOfferValidatorDatum :: P.String -> P.IO SellOfferT.ValidatorDatum
readStringDecodedAsSellOfferValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @SellOfferT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsSellOfferPolicyRedeemer "{\"getRedeemer\":\"d87e9fd8799f0a19021705d8799fd8799f9fd8799f581c70f7fee602900e395d84c1a3cfe1603303fb8c48435777be6455a8bc44343635341836ffffff1b0000018a52a7ad15ff5840529e9eb87717a6fbe10203615a465e50eb99ae2908e6ab0dc94302ec18f4b787b24aefce16b772795dcfde40947037952fe91888919ed06049c2362ed1e19a0dffff\"}"

readStringDecodedAsSellOfferPolicyRedeemer :: P.String -> P.IO SellOfferT.PolicyRedeemer
readStringDecodedAsSellOfferPolicyRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @SellOfferT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

