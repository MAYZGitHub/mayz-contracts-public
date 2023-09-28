{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Script.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad                       as Monad
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified Ledger                              (PaymentPubKeyHash (..), pubKeyHashAddress, unPaymentPubKeyHash, unStakePubKeyHash)
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.OffChain         as LedgerConstraintsOffChain
import qualified Ledger.Constraints.TxConstraints    as LedgerTxConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Tx                           as LedgerTx
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
import qualified Protocol.Constants                  as Constants
import qualified Protocol.OffChainHelpers            as OffChainHelpers
import qualified Protocol.PABTypes                   as T
import qualified Protocol.Protocol.Types             as ProtocolT
import qualified Protocol.Script.Types               as T
import qualified Protocol.Types                      as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

class AttachScriptToTx a where
    attachScriptToTx :: a -> LedgerConstraintsOffChain.ScriptLookups b

instance AttachScriptToTx LedgerApiV2.MintingPolicy where
    attachScriptToTx :: LedgerApiV2.MintingPolicy -> LedgerConstraintsOffChain.ScriptLookups b
    attachScriptToTx = LedgerConstraints.plutusV2MintingPolicy

instance AttachScriptToTx LedgerApiV2.Validator where
    attachScriptToTx :: LedgerApiV2.Validator -> LedgerConstraintsOffChain.ScriptLookups b
    attachScriptToTx = LedgerConstraints.plutusV2OtherScript

--------------------------------------------------------------------------------2

endPointProtocolScriptAdd :: T.PABProtocolScriptAddParams -> PlutusContract.Contract w s DataText.Text ()
endPointProtocolScriptAdd T.PABProtocolScriptAddParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Protocol Script Add"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = ppsapProtocolPABParams
        ---------------------
        !protocolPolicyID = T.pppProtocolPolicyID protocolPABParams
        !protocolPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting protocolPolicyID
        !protocolValidator = T.pppProtocolValidator protocolPABParams
        !protocolValidator_ScriptHash = OffChainHelpers.hashScriptValidator protocolValidator
        ---------------------
        !scriptPolicyID = T.pppScriptPolicyID protocolPABParams
        !scriptPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting scriptPolicyID
        !scriptValidator = T.pppScriptValidator protocolPABParams
        !scriptValidator_ScriptHash = OffChainHelpers.hashScriptValidator scriptValidator
        ---------------------
        !fundValidator = T.ffppFundValidator $ head $ T.pppFundFactoryPABParams protocolPABParams
        !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
        !investUnitValidator = T.ffppInvestUnitValidator $ head $ T.pppFundFactoryPABParams protocolPABParams
        !investUnitValidator_ScriptHash = OffChainHelpers.hashScriptValidator investUnitValidator
        ---------------------
        !scriptsPolicyToAddTotal = [(protocolPolicyID, protocolPolicyID_ScriptHash), (scriptPolicyID, scriptPolicyID_ScriptHash)]
        !scriptsValidatorsToAddTotal = [(protocolValidator, protocolValidator_ScriptHash),
            (fundValidator, fundValidator_ScriptHash),
            (investUnitValidator, investUnitValidator_ScriptHash),
            (scriptValidator, scriptValidator_ScriptHash)
            ]
        ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script To Add Total: %s" (P.show $ length scriptsPolicyToAddTotal + length scriptsValidatorsToAddTotal)
    ---------------------
    doAddScriptTxWitParams nameEndPoint protocolPABParams Nothing 1 scriptsPolicyToAddTotal scriptsValidatorsToAddTotal

--------------------------------------------------------------------------------2

endPointProtocolScriptDelete :: T.PABProtocolScriptDeleteParams -> PlutusContract.Contract w s DataText.Text ()
endPointProtocolScriptDelete T.PABProtocolScriptDeleteParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Protocol Script Delete"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = ppsdpProtocolPABParams
        ---------------------
        !protocolPolicyID = T.pppProtocolPolicyID protocolPABParams
        !protocolPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting protocolPolicyID
        !protocolValidator = T.pppProtocolValidator protocolPABParams
        !protocolValidator_ScriptHash = OffChainHelpers.hashScriptValidator protocolValidator
        ---------------------
        !fundValidator = T.ffppFundValidator $ head $ T.pppFundFactoryPABParams protocolPABParams
        !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
        !investUnitValidator = T.ffppInvestUnitValidator $ head $ T.pppFundFactoryPABParams protocolPABParams
        !investUnitValidator_ScriptHash = OffChainHelpers.hashScriptValidator investUnitValidator
        ---------------------
        !scriptPolicyID = T.pppScriptPolicyID protocolPABParams
        !scriptPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting scriptPolicyID
        !scriptValidator = T.pppScriptValidator protocolPABParams
        !scriptValidator_ScriptHash = OffChainHelpers.hashScriptValidator scriptValidator
        ---------------------
        !scriptsPolicyToDeleteTotal = [(protocolPolicyID, protocolPolicyID_ScriptHash), (scriptPolicyID, scriptPolicyID_ScriptHash)]
        !scriptsValidatorsToDeleteTotal = [
            (protocolValidator, protocolValidator_ScriptHash),
             (fundValidator, fundValidator_ScriptHash),
              (investUnitValidator, investUnitValidator_ScriptHash),
               (scriptValidator, scriptValidator_ScriptHash)
               ]
        ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script To Delete Total: %s" (P.show $ length scriptsPolicyToDeleteTotal + length scriptsValidatorsToDeleteTotal)
    ---------------------
    doDeleteScriptTxWithParams nameEndPoint protocolPABParams Nothing 2 scriptsPolicyToDeleteTotal scriptsValidatorsToDeleteTotal

--------------------------------------------------------------------------------2

endPointFundScriptAdd :: T.PABFundScriptAddParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundScriptAdd T.PABFundScriptAddParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
     ---------------------
    let nameEndPoint = "Fund Script Add"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfsapProtocolPABParams
        !fundPABParams  = pfsapFundPABParams
        ---------------------
    let
        !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
        ---------------------
        !fundHoldingPolicyID = T.fppFundHoldingPolicyID fundPABParams
        !fundHoldingPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting fundHoldingPolicyID
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        ---------------------
        !scriptsPolicyToAddTotal = [(fundPolicy, fundPolicy_ScriptHash), (fundHoldingPolicyID, fundHoldingPolicyID_ScriptHash)]
        !scriptsValidatorsToAddTotal = [(fundHoldingValidator, fundHoldingValidator_ScriptHash)]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script To Add Total: %s" (P.show $ length scriptsPolicyToAddTotal + length scriptsValidatorsToAddTotal)
    ---------------------
    doAddScriptTxWitParams nameEndPoint protocolPABParams (Just fundPABParams) 1 scriptsPolicyToAddTotal scriptsValidatorsToAddTotal

--------------------------------------------------------------------------------2

endPointFundScriptDelete :: T.PABFundScriptDeleteParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundScriptDelete T.PABFundScriptDeleteParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Fund Script Delete"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfsdpProtocolPABParams
        !fundPABParams  = pfsdpFundPABParams
        ---------------------
    let
        !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
        ---------------------
        !fundHoldingPolicyID = T.fppFundHoldingPolicyID fundPABParams
        !fundHoldingPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting fundHoldingPolicyID
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        ---------------------
        !scriptsPolicyToDeleteTotal = [(fundPolicy, fundPolicy_ScriptHash), (fundHoldingPolicyID, fundHoldingPolicyID_ScriptHash)]
        !scriptsValidatorsToDeleteTotal = [(fundHoldingValidator, fundHoldingValidator_ScriptHash)]
        ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script To Delete Total: %s" (P.show $ length scriptsPolicyToDeleteTotal + length scriptsValidatorsToDeleteTotal)
    ---------------------
    doDeleteScriptTxWithParams nameEndPoint protocolPABParams (Just fundPABParams) 2 scriptsPolicyToDeleteTotal scriptsValidatorsToDeleteTotal

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

doAddScriptTxWitParams :: P.String
    -> T.ProtocolPABParams -> Maybe T.FundPABParams
    -> Integer
    -> [(LedgerApiV2.MintingPolicy, LedgerApiV2.ScriptHash)]
    -> [(LedgerApiV2.Validator, LedgerApiV2.ScriptHash)]
    -> PlutusContract.Contract w s DataText.Text ()
doAddScriptTxWitParams
        nameEndPoint protocolPABParams fundPABParams'
        quantity scriptsPolicyToAddTotal scriptsValidatorsToAddTotal = do
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let !user = Ledger.unPaymentPubKeyHash userPPKH
        !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
        !userAddressStakingCredential = case OffChainHelpers.getStakePubKeyHash userAdds of
            Nothing              -> Nothing
            Just stakePubKeyHash -> Just $ Ledger.unStakePubKeyHash stakePubKeyHash
    userAddsCardano <- PlutusContract.ownAddress
    -- !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    let !scriptPolicyID = T.pppScriptPolicyID protocolPABParams
        !scriptPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting scriptPolicyID
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxOs_With_ScriptDatums <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @T.ValidatorDatum @T.ScriptDatumType scriptPolicyID_CS uTxOsAt_ScriptValidator T.getScriptDatumType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "uTxOs_With_ScriptDatums: %s" (P.show [txOutRef | (txOutRef, _, _) <- uTxOs_With_ScriptDatums])
    ---------------------
    let isPresentInUTxO :: (d, LedgerApiV2.ScriptHash) -> PlutusContract.Contract w s DataText.Text Bool
        isPresentInUTxO (_, hash) =
            do
                let !scriptID_TN = LedgerApiV2.TokenName $ LedgerApiV2.getScriptHash hash
                    !scriptID_AC = LedgerValue.AssetClass (scriptPolicyID_CS, scriptID_TN)
                !utxo <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @T.ValidatorDatum @T.ScriptDatumType scriptID_AC uTxOsAt_ScriptValidator T.getScriptDatumType
                case utxo of
                    [] -> return False
                    _  -> return True
    scriptsPolicyToAddRemaining <- Monad.filterM (fmap not . isPresentInUTxO) scriptsPolicyToAddTotal
    scriptsValidatorsToAddRemaining <- Monad.filterM (fmap not . isPresentInUTxO) scriptsValidatorsToAddTotal
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script To Add Remaining: %s" (P.show $ length scriptsPolicyToAddRemaining + length scriptsValidatorsToAddRemaining)
    ---------------------
    let doAddScriptTx :: [(LedgerApiV2.MintingPolicy, LedgerApiV2.ScriptHash)] -> [(LedgerApiV2.Validator, LedgerApiV2.ScriptHash)] -> PlutusContract.Contract w s DataText.Text ()
        doAddScriptTx [] [] = return ()
        doAddScriptTx scriptsPolicyToAddLeft scriptsValidatorsToAddLeft = do
            ---------------------
            OffChainHelpers.printTitle (nameEndPoint ++ " : NEW ADD BATCH")
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Adding quantity: %s of %s" (P.show quantity) (P.show $ length scriptsPolicyToAddLeft + length scriptsValidatorsToAddLeft)
            ---------------------
            !uTxOsAt_ScriptValidator' <- PlutusContract.utxosAt scriptValidator_AddressCardano
            ---------------------
            -- Check if there is in the net the policy needed to burn the ScriptID tokens. If yes can use as ref, if not we attached the policy to the transaction
            !scriptRef_With_ScriptPolicyID' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "ScriptPolicyID" scriptPolicyID_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator'
            ---------------------
            !uTxOsAtUser' <- PlutusContract.utxosAt userAddsCardano
            ---------------------
            let scriptsPolicyToAddUsing = take quantity scriptsPolicyToAddLeft
                quantityLeft = quantity - P.fromIntegral (P.length scriptsPolicyToAddUsing)
                scriptsValidatorsToAddUsing = take quantityLeft scriptsValidatorsToAddLeft
                scriptsPolicyToAddLeftForNext = P.drop (P.fromIntegral quantity) scriptsPolicyToAddLeft
                scriptsValidatorsToAddLeftForNext = P.drop (P.fromIntegral quantityLeft) scriptsValidatorsToAddLeft
                ---------------------
            let !intervalOffset1 = 1000
                !intervalOffset2 = T.validTimeRange - 1000
                !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
                ---------------------
            let prepareTx :: (AttachScriptToTx d) => (d, LedgerApiV2.ScriptHash) ->
                            PlutusContract.Contract w s DataText.Text (LedgerConstraintsOffChain.ScriptLookups a, LedgerTxConstraints.TxConstraints DataVoid.Void DataVoid.Void)
                prepareTx (script, hash) = do
                    ---------------------
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Preparing Tx for Adding Script: %s" (P.show hash)
                    ---------------------
                    let
                        !scriptID_TN = LedgerApiV2.TokenName $ LedgerApiV2.getScriptHash hash
                        !scriptID_AC = LedgerValue.AssetClass (scriptPolicyID_CS, scriptID_TN)
                        ---------------------
                        !valueFor_Mint_ScriptID = LedgerValue.assetClassValue scriptID_AC 1
                        ---------------------
                        !valueFor_ScriptDatum' = valueFor_Mint_ScriptID
                        !minADA_For_ScriptDatum = OnChainHelpers.calculateMinADAOfValue valueFor_ScriptDatum' True
                        !value_MinADA_For_ScriptDatum = LedgerAda.lovelaceValueOf minADA_For_ScriptDatum
                        !valueFor_ScriptDatum = LedgerAda.lovelaceValueOf 100000000 <> valueFor_ScriptDatum' <> value_MinADA_For_ScriptDatum
                        ---------------------
                        !scriptDatum_Out = T.mkScriptDatum Nothing user userAddressStakingCredential (LedgerApiV2.getScriptHash hash)
                        ---------------------
                    let !redeemer_For_Mint_ScriptID = T.PolicyRedeemerMintID T.PolicyRedeemerMintIDType -- user Nothing
                    ---------------------
                    case scriptRef_With_ScriptPolicyID' of
                        Nothing -> do
                            -- I dont attach the policy here... i added in the end, becasuse ill use this many times to create the tx... Ill add in the end to be sure that is added only once
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Mint Token With Attached ScriptPolicyID (but not attaching it yet): %s" (P.show valueFor_Mint_ScriptID)
                        Just _ -> do
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Mint Token With Ref ScriptPolicyID: %s" (P.show valueFor_Mint_ScriptID)
                    ---------------------
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
                    ---------------------
                    let
                        (lookupsTx_Mint_ScriptID, tx_Mint_ScriptID) =
                            OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_ScriptID Nothing  (Just redeemer_For_Mint_ScriptID) scriptRef_With_ScriptPolicyID' Nothing
                    ---------------------
                        lookupsTxPrepare =
                            lookupsTx_Mint_ScriptID
                                P.<> attachScriptToTx script
                        txPrepare =
                            tx_Mint_ScriptID
                                P.<> LedgerConstraints.mustPayToAddressWithReferenceScript scriptValidator_Address hash (Just $ LedgerTxConstraints.TxOutDatumInTx $ LedgerApiV2.Datum $ PlutusTx.toBuiltinData scriptDatum_Out) valueFor_ScriptDatum
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
                    ---------------------
                    return (lookupsTxPrepare, txPrepare)
            ---------------------
            !res1 <- mapM prepareTx scriptsPolicyToAddUsing
            !res2 <- mapM prepareTx scriptsValidatorsToAddUsing
            ---------------------
            let !lookupsTx1 = fst <$> res1
                !lookupsTx2 = fst <$> res2
            ---------------------
            checkScriptPolicyID <- case scriptRef_With_ScriptPolicyID' of
                Nothing -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptPolicyID was not found as ref. Attaching Script: %s" (P.show scriptPolicyID_ScriptHash)
                    return [attachScriptToTx scriptPolicyID]
                Just _ -> do 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptPolicyID was found as ref. Adding the Ref only if it is not already in some input. Script: %s" (P.show scriptPolicyID_ScriptHash)
                    return []
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            ---------------------
            let !lookupsTx3 = checkScriptPolicyID
            ---------------------
                !lookupsTx =
                    foldl (P.<>) (LedgerConstraints.unspentOutputs uTxOsAtUser') (lookupsTx1 ++ lookupsTx2 ++ lookupsTx3) -- ++ lookupsTx3 ++ lookupsTx4
                !tx =
                    mconcat (snd <$> res1)
                        P.<> mconcat (snd <$> res2)
                        P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                        P.<> LedgerConstraints.mustBeSignedBy userPPKH
            ------------------------
            OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams fundPABParams' lookupsTx tx
            ------------------------
            doAddScriptTx scriptsPolicyToAddLeftForNext scriptsValidatorsToAddLeftForNext
            ------------------------
    doAddScriptTx scriptsPolicyToAddRemaining scriptsValidatorsToAddRemaining

--------------------------------------------------------------------------------2

doDeleteScriptTxWithParams :: P.String  -> T.ProtocolPABParams  -> Maybe T.FundPABParams
    -> Integer
    -> [(LedgerApiV2.MintingPolicy, LedgerApiV2.ScriptHash)]
    -> [(LedgerApiV2.Validator, LedgerApiV2.ScriptHash)]
    -> PlutusContract.Contract w s DataText.Text ()
doDeleteScriptTxWithParams
        nameEndPoint protocolPABParams fundPABParams'
        quantity scriptsPolicyToDeleteTotal scriptsValidatorsToDeleteTotal = do
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator_Address = T.ffppFundValidator_Address $ head $ T.pppFundFactoryPABParams protocolPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !scriptPolicyID = T.pppScriptPolicyID protocolPABParams
        !scriptPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting scriptPolicyID
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator = T.pppScriptValidator protocolPABParams
        !scriptValidator_ScriptHash = OffChainHelpers.hashScriptValidator scriptValidator
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, Constants.protocolID_TN)
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    ---------------------
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    -- Check if there is in the net the utxo with ProtocolDatum, I need it for ref Input. throwError if not.
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    ---------------------
    !uTxO_With_FundDatum' <- case fundPABParams' of
            Just fundPABParams -> do
                let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, Constants.fundID_TN)
                ---------------------
                !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
                ---------------------
                !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
                return $ Just uTxO_With_FundDatum
            Nothing -> return Nothing
    ---------------------
    !uTxOs_With_ScriptDatums <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @T.ValidatorDatum @T.ScriptDatumType scriptPolicyID_CS uTxOsAt_ScriptValidator T.getScriptDatumType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "UTxOs with ScriptDatum: %s" (P.show [txOutRef | (txOutRef, _, _) <- uTxOs_With_ScriptDatums])
    ---------------------
    let isPresentInUTxO :: (d, LedgerApiV2.ScriptHash) -> PlutusContract.Contract w s DataText.Text Bool
        isPresentInUTxO (_, hash) =
            do
                let !scriptID_TN = LedgerApiV2.TokenName $ LedgerApiV2.getScriptHash hash
                    !scriptID_AC = LedgerValue.AssetClass (scriptPolicyID_CS, scriptID_TN)
                !utxo <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @T.ValidatorDatum @T.ScriptDatumType scriptID_AC uTxOsAt_ScriptValidator T.getScriptDatumType
                case utxo of
                    [] -> return False
                    _  -> return True
    ---------------------
    scriptsPolicyToDeleteRemaining <- Monad.filterM isPresentInUTxO scriptsPolicyToDeleteTotal
    scriptsValidatorsToDeleteRemaining <- Monad.filterM isPresentInUTxO scriptsValidatorsToDeleteTotal
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Script To Delete Remaining: %s" (P.show $ length scriptsPolicyToDeleteRemaining + length scriptsValidatorsToDeleteRemaining)
    ---------------------
    let doDeleteScriptTx ::  [(LedgerApiV2.MintingPolicy, LedgerApiV2.ScriptHash)] -> [(LedgerApiV2.Validator, LedgerApiV2.ScriptHash)] -> PlutusContract.Contract w s DataText.Text ()
        doDeleteScriptTx [] [] = return ()
        doDeleteScriptTx scriptsPolicyToDeleteLeft scriptsValidatorsToDeleteLeft = do
            ---------------------
            OffChainHelpers.printTitle (nameEndPoint ++ " : NEW DELETE BATCH")
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Deleting quantity: %s of %s" (P.show quantity) (P.show $ length scriptsPolicyToDeleteLeft + length scriptsValidatorsToDeleteLeft)
            ---------------------
            !uTxOsAt_ScriptValidator' <- PlutusContract.utxosAt scriptValidator_AddressCardano
            ---------------------
            -- Check if there is in the net the policy needed to burn the ScriptID tokens. If yes can use as ref, if not we attached the policy to the transaction
            !scriptRef_With_ScriptPolicyID' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "ScriptPolicyID" scriptPolicyID_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator'
            ---------------------
            -- Check if there is in the net the validator needed to run when consuming utxos with scripts. If yes we can use as ref, if not we attached the policy to the transaction
            !scriptRef_With_ScriptValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "ScriptValidator" scriptValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator'
            ---------------------
            !uTxOsAtUser' <- PlutusContract.utxosAt userAddsCardano
            ---------------------
            let scriptsPolicyToDeleteUsing = take quantity scriptsPolicyToDeleteLeft
                quantityLeft = quantity - P.fromIntegral (P.length scriptsPolicyToDeleteUsing)
                scriptsValidatorsToDeleteUsing = take quantityLeft scriptsValidatorsToDeleteLeft
                scriptsPolicyToDeleteLeftForNext = P.drop (P.fromIntegral quantity) scriptsPolicyToDeleteLeft
                scriptsValidatorsToDeleteLeftForNext = P.drop (P.fromIntegral quantityLeft) scriptsValidatorsToDeleteLeft
            ---------------------
            let !intervalOffset1 = 1000
                !intervalOffset2 = T.validTimeRange - 1000
                !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
            ---------------------
            let prepareTx :: (d, LedgerApiV2.ScriptHash) ->
                            PlutusContract.Contract w s DataText.Text (LedgerConstraintsOffChain.ScriptLookups a, LedgerTxConstraints.TxConstraints DataVoid.Void DataVoid.Void)
                prepareTx  (_, hash) = do
                    let !scriptID_TN = LedgerApiV2.TokenName $ LedgerApiV2.getScriptHash hash
                        !scriptID_AC = LedgerValue.AssetClass (scriptPolicyID_CS, scriptID_TN)
                    ---------------------
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Preparing Tx for Deleting Script: %s" (P.show hash)
                    ---------------------
                    !utxos <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_AC @T.ValidatorDatum @T.ScriptDatumType scriptID_AC uTxOsAt_ScriptValidator' T.getScriptDatumType
                    !utxo <- case utxos of
                        [] -> PlutusContract.throwError "No UTxO for Deleting in found"
                        _  -> return $ head utxos
                    ---------------------
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Preparing Tx for Deleting in: %s" (P.show utxo)
                    ---------------------
                    let !scriptDatum_In = (\(_, _, dat) -> dat) utxo
                        ---------------------
                        !valueFor_Burn_ScriptID = LedgerValue.assetClassValue scriptID_AC (negate 1)
                        !valueOf_ScriptDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) utxo
                        ---------------------
                        adminPaymentPKH = T.sdAdminPaymentPKH scriptDatum_In
                        valueFor_Master = LedgerValue.adaOnlyValue valueOf_ScriptDatum_In
                        ---------------------
                    let !redeemer_For_Burn_ScriptID = T.PolicyRedeemerBurnID T.PolicyRedeemerBurnIDType -- user Nothing
                    let !redeemer_For_Consuming_ScriptDatum = T.ValidatorRedeemerScriptDelete
                    ---------------------
                    case scriptRef_With_ScriptPolicyID' of
                        Nothing -> do
                            -- I dont attach the policy here... i added in the end, becasuse ill use this many times to create the tx... Ill add in the end to be sure that is added only once
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Burn Token With Attached ScriptPolicyID (but not attaching it yet): %s" (P.show valueFor_Burn_ScriptID)
                        Just _ -> do
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Burn Token With Ref ScriptPolicyID: %s" (P.show valueFor_Burn_ScriptID)
                    ---------------------
                    case scriptRef_With_ScriptValidator' of
                        Nothing -> do
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Speend the utxo with the attached ScriptValidator (but not attaching it yet)"
                        Just _ -> do
                            PlutusContract.logInfo @P.String $ TextPrintf.printf "Speend the utxo with the ref ScriptValidator"
                    ---------------------
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
                    ---------------------
                    let (lookupsTx_Burn_ScriptID, tx_Burn_ScriptID) =
                            OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Burn_ScriptID Nothing (Just redeemer_For_Burn_ScriptID) scriptRef_With_ScriptPolicyID' Nothing
                    let (lookupsTx_Consume_ScriptDatum, tx_Consume_ScriptDatum) =
                            OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' utxo redeemer_For_Consuming_ScriptDatum scriptRef_With_ScriptValidator' Nothing
                    ---------------------
                    let lookupsTx = lookupsTx_Burn_ScriptID P.<> lookupsTx_Consume_ScriptDatum
                    let tx = tx_Burn_ScriptID P.<> tx_Consume_ScriptDatum P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash adminPaymentPKH) valueFor_Master
                    return (lookupsTx, tx)
                ---------------------
            !res1 <- mapM prepareTx scriptsPolicyToDeleteUsing
            !res2 <- mapM prepareTx scriptsValidatorsToDeleteUsing
            ---------------------
            let !lookupsTx1 = fst <$> res1
                !lookupsTx2 = fst <$> res2
            ---------------------
            checkScriptValidator <- case scriptRef_With_ScriptValidator' of
                Nothing -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptValidator was not found as ref. Attaching Script: %s" (P.show scriptValidator_ScriptHash)
                    return [attachScriptToTx scriptValidator]
                Just _ -> do 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptValidator was found as ref. Adding the Ref only if it is not already in some input. Script: %s" (P.show scriptValidator_ScriptHash)
                    return []
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            ---------------------
            checkScriptPolicyID <- case scriptRef_With_ScriptPolicyID' of
                Nothing -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptPolicyID was not found as ref. Attaching Script: %s" (P.show scriptPolicyID_ScriptHash)
                    return [attachScriptToTx scriptPolicyID]
                Just _ -> do 
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "ScriptPolicyID was found as ref. Adding the Ref only if it is not already in some input. Script: %s" (P.show scriptPolicyID_ScriptHash)
                    return []
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            ---------------------
            let !lookupsTx3 = checkScriptValidator ++ checkScriptPolicyID
            ---------------------
            let !lookupsTx =
                    foldl (P.<>) (LedgerConstraints.unspentOutputs uTxOsAtUser') (lookupsTx1 ++ lookupsTx2 ++ lookupsTx3)
                         P.<> case uTxO_With_FundDatum' of
                                Nothing ->
                                    LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                                Just uTxO_With_FundDatum ->
                                    LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                !tx =
                    mconcat (snd <$> res1)
                        P.<> mconcat (snd <$> res2)
                        P.<> case uTxO_With_FundDatum' of
                                Nothing ->
                                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                                Just uTxO_With_FundDatum ->
                                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                        P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                        P.<> LedgerConstraints.mustBeSignedBy userPPKH
            ------------------------
            OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams fundPABParams' lookupsTx tx
            ------------------------
            doDeleteScriptTx scriptsPolicyToDeleteLeftForNext scriptsValidatorsToDeleteLeftForNext
        ------------------------
    doDeleteScriptTx scriptsPolicyToDeleteRemaining scriptsValidatorsToDeleteRemaining

--------------------------------------------------------------------------------2
