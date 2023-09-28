{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

--------------------------------------------------------------------------------2
module Protocol.Fund.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad                       as Monad
import qualified Data.Map                            as DataMap
import qualified Data.Set                            as Set
import qualified Data.Text                           as DataText (Text)
import qualified Ledger
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Ada                          as LedgerValue
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Crypto                       as Crypto
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified PlutusTx.Ratio                      as TxRatio
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.Constants                   as T
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Generic.Types                       as T
import qualified Protocol.Constants                  as T
import qualified Protocol.Fund.Helpers               as Helpers
import qualified Protocol.Fund.Holding.Types         as FundHoldingT
import qualified Protocol.Fund.Types                 as FundT
import qualified Protocol.Fund.Types                 as T
import qualified Protocol.InvestUnit.Types           as InvestUnitT
import qualified Protocol.OffChainHelpers            as OffChaiHelpers
import qualified Protocol.OffChainHelpers            as OffChainHelpers
import qualified Protocol.OnChainHelpers             as OnChainHelpers
import qualified Protocol.PABTypes                   as T
import qualified Protocol.Protocol.Helpers           as Helpers
import qualified Protocol.Protocol.Types             as ProtocolT
import qualified Protocol.Types                      as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

endPointFundPrepare :: T.PABFundPrepareParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundPrepare T.PABFundPrepareParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Fund Prepare"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfppProtocolPABParams
    let !fundPABParams = pfppFundPABParams
    ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
    ---------------------
    let !investUnitValidator_Hash = T.fppInvestUnitValidator_Hash fundPABParams
    ---------------------
    let !fundPolicy_Params = T.fppFundPolicy_Params fundPABParams
        !fundPolicy_TxOutRef = T.ppFundPolicy_TxOutRef fundPolicy_Params
    ---------------------
    let !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
    ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address

    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    case find (\(txOutRef, _) -> txOutRef == fundPolicy_TxOutRef) (DataMap.toList uTxOsAtUser) of
        Nothing ->
            PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "%s : Can't find uTxO for mint FundPolicy" nameEndPoint
        Just _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "%s : uTxO for mint FundPolicy found!" nameEndPoint
    ---------------------
    let fundPolicy_TxOut = head [(t, ci) | (t, ci) <- DataMap.toList uTxOsAtUser, t == fundPolicy_TxOutRef]
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !scriptRef_With_fundPolicy' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundPolicy" fundPolicy_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    ---------------------
    let !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
        !fundClasses = ProtocolT.pdFundClasses protocolDatum_In
        !selectedFuncClass' = find (\fundClass -> ProtocolT.fcIndex fundClass == pfppFundClassIndex) fundClasses
    ---------------------
    !requiredMAYZ <- case selectedFuncClass' of
            Nothing ->
                PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "%s : Can't find Fund Class: %s" nameEndPoint (P.show selectedFuncClass')
            Just selectedFuncClass -> do
                PlutusContract.logInfo @P.String $ TextPrintf.printf "%s : Fund Class: %s" nameEndPoint (P.show selectedFuncClass)
                PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
                return $ ProtocolT.fcRequiredMAYZ selectedFuncClass
    ---------------------
    let !tokenMAYZ_AC = LedgerValue.AssetClass (T.tokenMAYZ_CS, T.tokenMAYZ_TN)
        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
    ---------------------
    let !valueFor_Mint_FundID = LedgerValue.assetClassValue fundID_AC 1
        !valueFor_Mint_InvestUnitID = LedgerValue.assetClassValue investUnitID_AC 1
    ---------------------
        !valueFor_Mint_FundID_And_OtherIDs = valueFor_Mint_FundID <> valueFor_Mint_InvestUnitID
    ---------------------
        !valueFor_FundDatum' = valueFor_Mint_FundID
        !minADA_For_FundDatum = OnChainHelpers.calculateMinADAOfValue valueFor_FundDatum' True
        !value_MinADA_For_FundDatum = LedgerAda.lovelaceValueOf minADA_For_FundDatum
        !valueFor_FundDatum = valueFor_FundDatum' <> value_MinADA_For_FundDatum <> valueOf_RequiredMAYZ <> LedgerAda.lovelaceValueOf 5_000_000 -- TODO: min ada para datum grande
    ---------------------
        !valueFor_InvestUnitDatum' = valueFor_Mint_InvestUnitID
        !minADA_For_InvestUnitDatum = OnChainHelpers.calculateMinADAOfValue valueFor_InvestUnitDatum' True
        !value_MinADA_For_InvestUnitDatum = LedgerAda.lovelaceValueOf minADA_For_InvestUnitDatum
        !valueFor_InvestUnitDatum = valueFor_InvestUnitDatum' <> value_MinADA_For_InvestUnitDatum <> LedgerAda.lovelaceValueOf 5_000_000 -- TODO: min ada para datum grande
    ---------------------
        !admins = pfppAdmins
        !fundClass = pfppFundClassIndex
        !beginAt = pfppBeginAt
        !deadline = pfppDeadline
        !closedAt = pfppClosedAt
        !commission = pfppCommissionsPerYearInBPx1e3
        !holdingsCount = 0
        !holdingsIndex = 0
        !holdingsCreators = []
    ---------------------
        !fundDatum_Out = FundT.mkFundDatum
            fundPolicy_CS
            fundValidator_Hash
            fundHoldingPolicyID_CS
            fundHoldingValidator_Hash
            investUnitValidator_Hash
            admins fundClass beginAt deadline closedAt commission holdingsCount holdingsIndex holdingsCreators minADA_For_FundDatum
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundDatum: %s" (P.show valueFor_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !investUnit = pfppInvestUnit
        !investUnitDatum_Out = InvestUnitT.mkInvestUnitDatum fundPolicy_CS investUnit minADA_For_InvestUnitDatum
    ---------------------
    let !redeemer_For_Mint_FundID_And_OtherIDs = FundT.PolicyRedeemerMintID FundT.PolicyRedeemerMintIDType -- user Nothing
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    do
        let (lookupsTx_Mint_FundAndOthersID, tx_Mint_FundAndOthersID) = OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_FundID_And_OtherIDs (Just fundPolicy_TxOut) (Just redeemer_For_Mint_FundID_And_OtherIDs) scriptRef_With_fundPolicy' (Just fundPolicy)
        let
            lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> lookupsTx_Mint_FundAndOthersID
            tx =
                    tx_Mint_FundAndOthersID
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum investUnitValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData investUnitDatum_Out) valueFor_InvestUnitDatum
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ------------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams)  lookupsTx tx


--------------------------------------------------------------------------------2

endPointFundUpdate :: T.PABFundUpdateParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundUpdate T.PABFundUpdateParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Fund Update"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfupProtocolPABParams
    let !fundPABParams = pfupFundPABParams
        ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator = T.fppFundValidator fundPABParams
        !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
        !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    ---------------------
    !scriptRef_With_FundValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundValidator" fundValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !valueOf_FundDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundDatum
        !valueFor_FundDatum_Out = valueOf_FundDatum_In
    ---------------------
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    ---------------------
    let !admins = pfupAdmins
    ---------------------
        !fundDatum_Out =
            FundT.FundDatum $ Helpers.mkUpdated_FundDatum_With_NormalChanges fundDatum_In
                admins
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_In: %s" (P.show fundDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundDatum = FundT.ValidatorRedeemerDatumUpdate FundT.ValidatorRedeemerDatumUpdateType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundDatum: %s" (P.show redeemer_For_Consuming_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Consume_FundDatum, tx_Consume_FundDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundDatum redeemer_For_Consuming_FundDatum scriptRef_With_FundValidator' (Just fundValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> lookupsTx_Consume_FundDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> tx_Consume_FundDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundHoldingAdd :: T.PABFundHoldingAddParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundHoldingAdd T.PABFundHoldingAddParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Holding Add"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
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
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfhapProtocolPABParams
    let !fundPABParams = pfhapFundPABParams
    ---------------------
    let !fundValidator = T.fppFundValidator fundPABParams
        !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
        !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let !fundHoldingPolicyID = T.fppFundHoldingPolicyID fundPABParams
        !fundHoldingPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting fundHoldingPolicyID
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
    ---------------------
    let !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    ---------------------
    !scriptRef_With_FundValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundValidator" fundValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_FundHoldingPolicyID' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingPolicyID" fundHoldingPolicyID_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !valueOf_FundDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundDatum
        !valueFor_FundDatum_Out = valueOf_FundDatum_In
    ---------------------
    let !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    ---------------------
    let fundHolding_Index = FundT.fdHoldingsIndex fundDatum_In
        fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
        fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
    ---------------------
    let !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
    ---------------------
        !valueFor_FundHoldingDatum_Out' = valueFor_Mint_FundHoldingID
        !minADA_For_FundHoldingDatum_Out = OnChainHelpers.calculateMinADAOfValue valueFor_FundHoldingDatum_Out' True
        !value_MinADA_For_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_Out
        !valueFor_FundHoldingDatum_Out = valueFor_FundHoldingDatum_Out' <> value_MinADA_For_FundHoldingDatum_Out <> LedgerAda.lovelaceValueOf 10000000
        -- TODO: min ada paratodos los tokens quje tendra
    ---------------------
        !fundDatum_Out =
            FundT.FundDatum $ Helpers.mkUpdated_FundDatum_With_HoldingAdded fundDatum_In user userAddressStakingCredential minADA_For_FundHoldingDatum_Out
    ---------------------
        !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum $ FundHoldingT.FundHoldingDatumType  {
                        FundHoldingT.hdFundHolding_Index = FundT.fdHoldingsIndex fundDatum_In,
                        FundHoldingT.hdSubtotal_FT_Minted_Accumulated = 0,
                        FundHoldingT.hdSubtotal_FT_Minted = 0,
                        FundHoldingT.hdSubtotal_FT_Circulation = 0,
                        FundHoldingT.hdSubtotal_FT_ForComission = 0,
                        FundHoldingT.hdSubtotal_FT_ForComission_Acumulated = 0,
                        FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6  = 0,
                        FundHoldingT.hdSubtotal_Collected_Commissions_Protocol = 0,
                        FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ = 0,
                        FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins = 0,
                        FundHoldingT.hdMinADA = minADA_For_FundHoldingDatum_Out
                    }
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_In: %s" (P.show fundDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "holdingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundDatum = FundT.ValidatorRedeemerFundHoldingAdd $ FundT.ValidatorRedeemerFundHoldingAddType user userAddressStakingCredential minADA_For_FundHoldingDatum_Out
        !redeemer_For_Mint_FundHoldingID = FundHoldingT.PolicyRedeemerMintID FundHoldingT.PolicyRedeemerMintIDType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundDatum: %s" (P.show redeemer_For_Consuming_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Mint_FundHoldingID: %s" (P.show redeemer_For_Mint_FundHoldingID)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Mint_FundHoldingID, tx_Mint_FundHoldingID) =
                    OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_FundHoldingID Nothing (Just redeemer_For_Mint_FundHoldingID) scriptRef_With_FundHoldingPolicyID' (Just fundHoldingPolicyID)
        let (lookupsTx_Consume_FundDatum, tx_Consume_FundDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundDatum  redeemer_For_Consuming_FundDatum scriptRef_With_FundValidator' (Just fundValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> lookupsTx_Mint_FundHoldingID
                    P.<> lookupsTx_Consume_FundDatum
            tx =
                    tx_Mint_FundHoldingID
                    P.<> tx_Consume_FundDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum_Out
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundHoldingDelete :: T.PABFundHoldingDeleteParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundHoldingDelete _ = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Holding Delete"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"

--------------------------------------------------------------------------------2

endPointFundDeposit :: T.PABFundDepositParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundDeposit T.PABFundDepositParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Deposit"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfdpProtocolPABParams
    let !fundPABParams = pfdpFundPABParams
    ---------------------
    let !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !investUnitValidator_Address = T.fppInvestUnitValidator_Address fundPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let !deposit = pfdpAmount
    ---------------------
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_By_CS fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_FundPolicy' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundPolicy" fundPolicy_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    let !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
    ---------------------
    let !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
        !investUnitTokens = T.iuValues investUnit
    ---------------------
        !valueOf_TokensForDeposit = foldl (P.<>) (LedgerAda.lovelaceValueOf 0)
                [LedgerValue.assetClassValue
                    (LedgerValue.AssetClass
                        (cs, tn))
                    (amt * deposit) |
                    (cs, tn, amt) <- investUnitTokens]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_TokensForDeposit: %s" (P.show valueOf_TokensForDeposit)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    !valueOf_User <- OffChainHelpers.getBalanceOfUTXOs uTxOsAtUser
    ---------------------
    Monad.unless (OnChainHelpers.isIncludeValue valueOf_User valueOf_TokensForDeposit)
        P.$ PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "You dont have enough tokens to deposit"
    ---------------------
    let !deadline = FundT.fdDeadline fundDatum_In
        !commissionsPerYearInBPx1e3 = FundT.fdCommissionsPerYearInBPx1e3 fundDatum_In
    ---------------------
        !monthsRemaining =  Helpers.getRemainingMonths deadline now
    ---------------------
        !(userFT, commissionsFT, commissions_RatePerMonth_Numerator1e6) = Helpers.calculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 deadline now deposit
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "deposit: %s" (P.show deposit)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemaining: %s" (P.show monthsRemaining)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsPerYearInBPx1e3: %s" (P.show commissionsPerYearInBPx1e3)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "userFT: %s" (P.show userFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsFT: %s" (P.show commissionsFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissions_RatePerMonth_Numerator1e6: %s" (P.show commissions_RatePerMonth_Numerator1e6)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
    ---------------------
        !valueFor_Mint_FundFT = LedgerValue.assetClassValue fundFT_AC deposit
    ---------------------
        !valueFor_FT_User = LedgerValue.assetClassValue fundFT_AC userFT
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
    ---------------------
        !valueFor_User_Out' = valueFor_FT_User
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> valueOf_TokensForDeposit P.<> valueFor_FT_Commissions
    ---------------------
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum $ Helpers.mkUpdated_FundHoldingDatum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_RatePerMonth_Numerator1e6
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerDeposit $ FundHoldingT.ValidatorRedeemerDepositType now deposit
        !redeemer_For_Mint_FundFT = FundT.PolicyRedeemerMintFT FundT.PolicyRedeemerMintFTType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Mint_FundFT: %s" (P.show redeemer_For_Mint_FundFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Mint_FundFT, tx_Mint_FundFT) =
                    OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_FundFT Nothing  (Just redeemer_For_Mint_FundFT) scriptRef_With_FundPolicy' (Just fundPolicy)
        let (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_InvestUnitDatum])
                    P.<> lookupsTx_Mint_FundFT
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_InvestUnitDatum)
                    P.<> tx_Mint_FundFT
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundWithdraw :: T.PABFundWithdrawParams-> PlutusContract.Contract w s DataText.Text ()
endPointFundWithdraw T.PABFundWithdrawParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Withdraw"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfwpProtocolPABParams
    let !fundPABParams = pfwpFundPABParams
    ---------------------
    let !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !investUnitValidator_Address = T.fppInvestUnitValidator_Address fundPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
        ---------------------
    let !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let !withdraw = pfwpAmount
    ---------------------
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_FundPolicy' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundPolicy" fundPolicy_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    let !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
    ---------------------
    let !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
        !investUnitTokens = T.iuValues investUnit
    ---------------------
    let !deadline = FundT.fdDeadline fundDatum_In
        !commissionsPerYearInBPx1e3 = FundT.fdCommissionsPerYearInBPx1e3 fundDatum_In
     ---------------------
        !monthsRemaining =  Helpers.getRemainingMonths deadline now
    ---------------------
        !(commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_RatePerMonth_Numerator1e6) = Helpers.calculateWithdrawComissionsUsingMonths commissionsPerYearInBPx1e3 deadline now withdraw
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdraw: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemaining: %s" (P.show monthsRemaining)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsPerYearInBPx1e3: %s" (P.show commissionsPerYearInBPx1e3)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsForUserFTToGetBack: %s" (P.show commissionsForUserFTToGetBack)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawPlusCommissionsGetBack: %s" (P.show withdrawPlusCommissionsGetBack)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissions_RatePerMonth_Numerator1e6: %s" (P.show commissions_RatePerMonth_Numerator1e6)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ------------------
    let !valueOf_TokensForWithdraw = foldl (P.<>) (LedgerAda.lovelaceValueOf 0)
            [LedgerValue.assetClassValue
                (LedgerValue.AssetClass
                    (cs, tn))
                (amt * withdrawPlusCommissionsGetBack) |
                (cs, tn, amt) <- investUnitTokens]
    ---------------------
    let fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
    ---------------------
        !valueFor_Burn_FundFT = LedgerValue.assetClassValue fundFT_AC (negate withdrawPlusCommissionsGetBack)
    ---------------------
        !valueFor_FT_CommissionsToGetBack = LedgerValue.assetClassValue fundFT_AC commissionsForUserFTToGetBack
    ---------------------
        !valueFor_User_Out' = valueOf_TokensForWithdraw
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Enough_Subtotal_By_CS withdrawPlusCommissionsGetBack fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    let !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueOf_TokensForWithdraw P.<> negate valueFor_FT_CommissionsToGetBack
    ---------------------
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum $ Helpers.mkUpdated_FundHoldingDatum_With_Withdraw fundHoldingDatum_In withdraw commissionsForUserFTToGetBack commissions_RatePerMonth_Numerator1e6
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerWithdraw $ FundHoldingT.ValidatorRedeemerWithdrawType now withdraw
        !redeemer_For_Burn_FundFT = FundT.PolicyRedeemerBurnFT FundT.PolicyRedeemerBurnFTType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Burn_FundFT: %s" (P.show redeemer_For_Burn_FundFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Burn_FundFT, tx_Burn_FundFT) =
                    OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Burn_FundFT Nothing  (Just redeemer_For_Burn_FundFT) scriptRef_With_FundPolicy' (Just fundPolicy)
        let (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_InvestUnitDatum])
                    P.<> lookupsTx_Burn_FundFT
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_InvestUnitDatum)
                    P.<> tx_Burn_FundFT
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundReIndexing :: T.PABFundReIndexingParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundReIndexing T.PABFundReIndexingParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "ReIndexing"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfripProtocolPABParams
        !fundPABParams = pfripFundPABParams
     ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !investUnitValidator = T.fppInvestUnitValidator fundPABParams
        !investUnitValidator_Hash = T.fppInvestUnitValidator_Hash fundPABParams
        !investUnitValidator_ScriptHash = OffChainHelpers.hashScriptValidator investUnitValidator
        !investUnitValidator_Address = T.fppInvestUnitValidator_Address fundPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
        ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let !investUnitTokensToAdd = pfripTokensToAdd
        !investUnitTokensToRemove = pfripTokensToRemove
        !holdingTxOutRef = pfripFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !uTxOs_With_FundHoldingDatums <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator FundHoldingT.getFundHoldingDatumType
    let !uTxOs_With_FundHoldingDatums_WithoutSelected = filter (\(ref, _, _) -> ref /= holdingTxOutRef) uTxOs_With_FundHoldingDatums
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_InvestUnitValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "InvestUnitValidator" investUnitValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
        !investUnit_In = InvestUnitT.iudInvestUnit investUnitDatum_In
        !investUnitTokens_In = T.iuValues investUnit_In
    ---------------------
        !tokensToAdd = T.iuValues investUnitTokensToAdd
        !tokensToRemove = T.iuValues investUnitTokensToRemove
    ---------------------
    let !investUnit_Out' = OnChainHelpers.flattenValueAdd investUnitTokens_In tokensToAdd
        !investUnit_Out = OnChainHelpers.flattenValueSub investUnit_Out' tokensToRemove
    ---------------------
        !valueOf_InvestUnitDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_InvestUnitDatum
        !valueFor_InvestUnitDatum_Out = valueOf_InvestUnitDatum_In
    ---------------------
        !investUnitDatum_Out = InvestUnitT.mkInvestUnitDatum (InvestUnitT.iudFundPolicy_CS investUnitDatum_In) (T.InvestUnit investUnit_Out) (InvestUnitT.iudMinADA investUnitDatum_In)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "investUnitDatum_In: %s" (P.show investUnitDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "investUnitDatum_Out: %s" (P.show investUnitDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        tokensPricesToAddADA = [ (cs, tn, 100) | (cs, tn, _) <- tokensToAdd]
        tokensPricesToRemoveADA = [ (cs, tn, 100) | (cs, tn, _) <- tokensToRemove]
    ---------------------
        uniquePairs :: P.Ord cs => P.Ord tn => [(cs, tn, Integer)] -> [(cs, tn, Integer)]
        uniquePairs tokens =
            Set.toList $ Set.fromList [(cs, tn, amt) | (cs, tn, amt) <- tokens]
    ---------------------
        tokensPricesADA = T.InvestUnit { T.iuValues = uniquePairs (tokensPricesToAddADA ++ tokensPricesToRemoveADA)}
    ---------------------
        oracleReIdx_Data = T.OracleReIdx_Data {
            T.oridTokensPriceADA  = tokensPricesADA,
            T.oridTime    = now - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.oracleData_Valid_Time `divide` 2)
        }
    ---------------------
    let oracleWallet_XPriv =  Crypto.generateFromSeed' T.oracleWallet_Seed
        oracleReIdx_DataBBS = OnChainHelpers.oracleReIdxDataToBBS oracleReIdx_Data
    let oracleSignature = Crypto.sign' oracleReIdx_DataBBS oracleWallet_XPriv
    ---------------------
    let findPriceADA :: T.CS -> T.TN -> PlutusContract.Contract w s DataText.Text P.Integer
        findPriceADA cs tn =
            case find (\(cs', tn', _) -> cs' == cs && tn' == tn) ( T.iuValues tokensPricesADA) of
                Nothing            -> PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "No price found for %s.%s" (P.show cs) (P.show tn)
                Just (_, _, price) -> return price
    -------------------
    !priceADAOf_TokensForTokensToAdd <- Monad.foldM (\acc (cs, tn, amt) -> do
                    price <- findPriceADA cs tn
                    return (acc + amt * price)
                 ) 0 tokensToAdd
    -------------------
    !priceADAOf_TokensForTokensToRemove <- Monad.foldM (\acc (cs, tn, amt) -> do
                    price <- findPriceADA cs tn
                    return (acc + amt * price)
                 ) 0 tokensToRemove
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "priceADAOf_TokensForTokensToAdd: %s" (P.show priceADAOf_TokensForTokensToAdd)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "priceADAOf_TokensForTokensToRemove: %s" (P.show priceADAOf_TokensForTokensToRemove)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (priceADAOf_TokensForTokensToAdd /= priceADAOf_TokensForTokensToRemove)
        P.$ PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "Prices dont match"
    ---------------------
    let getSubtotalUI :: (LedgerApiV2.TxOutRef, Ledger.DecoratedTxOut, FundHoldingT.FundHoldingDatumType) -> P.Integer
        getSubtotalUI (_, _, !dat) = FundHoldingT.hdSubtotal_FT_Minted dat
        !totalDepositsIU = P.sum (getSubtotalUI <$> uTxOs_With_FundHoldingDatums)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "totalDepositsIU: %s" (P.show totalDepositsIU)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, am * totalDepositsIU) | (cs, tn, am) <- tokensToAdd ]
        !valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, am * totalDepositsIU) | (cs, tn, am) <- tokensToRemove  ]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_TotalTokensToAdd: %s" (P.show valueOf_TotalTokensToAdd)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_TotalTokensToRemove: %s" (P.show valueOf_TotalTokensToRemove)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
    ---------------------
    Monad.unless (OnChainHelpers.isIncludeValue valueOf_FundHoldingDatum_In valueOf_TotalTokensToRemove)
        P.$ PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "FundHolding selected does not have enough tokens to delete"
    ---------------------
    let !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> valueOf_TotalTokensToAdd P.<> negate valueOf_TotalTokensToRemove
    ---------------------
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
        !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum fundHoldingDatum_In
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_InvestUnitDatum = InvestUnitT.ValidatorRedeemerReIndexing $ InvestUnitT.ValidatorRedeemerReIndexingType
            (T.InvestUnit tokensToAdd)
            (T.InvestUnit tokensToRemove)
            oracleReIdx_Data
            oracleSignature
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_InvestUnitDatum: %s" (P.show redeemer_For_Consuming_InvestUnitDatum)
    ---------------------
    let !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerReIndexing $ FundHoldingT.ValidatorRedeemerReIndexingType (T.InvestUnit tokensToAdd) (T.InvestUnit tokensToRemove)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Consume_InvestUnitDatum, tx_Consume_InvestUnitDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_InvestUnitDatum redeemer_For_Consuming_InvestUnitDatum scriptRef_With_InvestUnitValidator' (Just investUnitValidator)
        let (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList ((\(ref, dec, _) -> (ref, dec)) <$> uTxOs_With_FundHoldingDatums_WithoutSelected) )
                    P.<> lookupsTx_Consume_InvestUnitDatum
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> mconcat [LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundHoldingDatum') | uTxO_With_FundHoldingDatum' <- uTxOs_With_FundHoldingDatums_WithoutSelected]
                    P.<> tx_Consume_InvestUnitDatum
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum investUnitValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData investUnitDatum_Out) valueFor_InvestUnitDatum_Out
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundCollect_Protocol_Commissions :: T.PABFundCollect_Protocol_CommissionsParams-> PlutusContract.Contract w s DataText.Text ()
endPointFundCollect_Protocol_Commissions T.PABFundCollect_Protocol_CommissionsParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Withdraw Protocol Commissions"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfwpcpProtocolPABParams
    let !fundPABParams = pfwpcpFundPABParams
        ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
        ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    let !withdraw = pfwpcpAmount
        !holdingTxOutRef = pfwpcpFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
    let !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
        !deadline = FundT.fdDeadline fundDatum_In
    ------------------
        !share = ProtocolT.pdShare_Protocol protocolDatum_In
        !sharePct = TxRatio.unsafeRatio share 10_000
        !taken = FundHoldingT.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In
        !available = Helpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken now
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawing: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "sharePct: %s" (OffChainHelpers.displayRational 4 sharePct)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "taken: %s" (P.show taken)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "available: %s" (P.show  available)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (available < withdraw) P.$ PlutusContract.throwError "Not enough commissions to withdraw"
    ---------------------
    let fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
    ---------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
    ---------------------
        !valueFor_User_Out' = valueFor_FT_Commissions
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueFor_FT_Commissions
    ---------------------
    let !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $ Helpers.mkUpdated_FundHoldingDatum_With_Collect_Protocol_Commissions fundHoldingDatum_In withdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerCollect_Protocol_Commissions $ FundHoldingT.ValidatorRedeemerCollect_Protocol_CommissionsType now withdraw
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundCollect_MAYZ_Commissions :: T.PABFundCollect_MAYZ_CommissionsParams-> PlutusContract.Contract w s DataText.Text ()
endPointFundCollect_MAYZ_Commissions T.PABFundCollect_MAYZ_CommissionsParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Withdraw MAYZ Commissions"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfwmcpProtocolPABParams
    let !fundPABParams = pfwmcpFundPABParams
        ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
        ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    let !withdraw = pfwmcpAmount
        !holdingTxOutRef = pfwmcpFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
    let !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
        !deadline = FundT.fdDeadline fundDatum_In
    ------------------
        !share = ProtocolT.pdShare_MAYZ protocolDatum_In
        !sharePct = TxRatio.unsafeRatio share 10_000
        !taken = FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In
        !available = Helpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken now
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawing: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "sharePct: %s" (OffChainHelpers.displayRational 4 sharePct)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "taken : %s" (P.show taken )
    PlutusContract.logInfo @P.String $ TextPrintf.printf "available: %s" (P.show  available)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (available < withdraw) P.$ PlutusContract.throwError "Not enough commissions to withdraw"
    ---------------------
    let fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
    ---------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
    ---------------------
        !valueFor_User_Out' = valueFor_FT_Commissions
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueFor_FT_Commissions
    ---------------------
    let !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $ Helpers.mkUpdated_FundHoldingDatum_With_Collect_MAYZ_Commissions fundHoldingDatum_In withdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerCollect_MAYZ_Commissions $ FundHoldingT.ValidatorRedeemerCollect_MAYZ_CommissionsType now withdraw
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2


endPointFundCollect_FundAdmins_Commissions :: T.PABFundCollect_FundAdmins_CommissionsParams-> PlutusContract.Contract w s DataText.Text ()
endPointFundCollect_FundAdmins_Commissions T.PABFundCollect_FundAdmins_CommissionsParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Withdraw Fund Admins Commissions"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let !protocolPABParams = pfwfcpProtocolPABParams
    let !fundPABParams = pfwfcpFundPABParams
        ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
        ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        ---------------------
    let !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    let !withdraw = pfwfcpAmount
        !holdingTxOutRef = pfwfcpFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
    let !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
        !deadline = FundT.fdDeadline fundDatum_In
    ------------------
        !share = ProtocolT.pdShare_FundAdmins protocolDatum_In
        !sharePct = TxRatio.unsafeRatio share 10_000
        !taken = FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In
        !available = Helpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken now
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawing: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "sharePct: %s" (OffChainHelpers.displayRational 4 sharePct)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "taken: %s" (P.show taken)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "available: %s" (P.show available)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (available < withdraw) P.$ PlutusContract.throwError "Not enough commissions to withdraw"
    ---------------------
    let fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS,  T.fundFT_TN)
    ---------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
    ---------------------
        !valueFor_User_Out' = valueFor_FT_Commissions
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueFor_FT_Commissions
    ---------------------
    let !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $ Helpers.mkUpdated_FundHoldingDatum_With_Collect_FundAdmins_Commissions fundHoldingDatum_In withdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerCollect_FundAdmins_Commissions $ FundHoldingT.ValidatorRedeemerCollect_FundAdmins_CommissionsType now withdraw
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                    OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let lookupsTx =
                    LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                    LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChaiHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2
