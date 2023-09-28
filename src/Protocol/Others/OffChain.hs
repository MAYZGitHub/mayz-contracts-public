{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Others.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified Ledger
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2
import qualified Generic.Constants                   as T
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Protocol.Others.PolicyFT            as OnChain
import qualified Protocol.Others.PolicyNFT           as OnChain
import qualified Protocol.PABTypes                   as T
import qualified Protocol.OffChainHelpers as OffChaiHelpers
import qualified Generic.OffChainEval as OffChainEval
import qualified Protocol.OffChainHelpers as OffChainHelpers
import qualified Protocol.Constants as T
import qualified Protocol.InvestUnit.Types as InvestUnitT
import qualified Control.Monad as Monad
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

endPointSplitUtxO :: T.PABSplitUtxOParams -> PlutusContract.Contract w s DataText.Text ()
endPointSplitUtxO T.PABSplitUtxOParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "SplitUtxO"
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
    let !splitAmount = psupSplitAmount
    ---------------------
        !valueFor_SplitAmount = LedgerAda.lovelaceValueOf splitAmount
    ---------------------
        lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtUser
        tx =
            LedgerConstraints.mustPayToPubKey userPPKH valueFor_SplitAmount
                P.<> LedgerConstraints.mustPayToPubKey userPPKH valueFor_SplitAmount
                P.<> LedgerConstraints.mustBeSignedBy userPPKH
    ---------------------
    let !eval_MintingPolicies = [ ]
        !eval_Validators = [ ]
        !eval_GetDatums = [ ]
    ---------------------
    OffChainEval.evalAndSubmitTx nameEndPoint eval_MintingPolicies eval_Validators eval_GetDatums lookupsTx tx
  
--------------------------------------------------------------------------------2

endPointMintFT :: T.PABMintFTParams -> PlutusContract.Contract w s DataText.Text ()
endPointMintFT T.PABMintFTParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Mint FT"
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
    let !mintPolicyNum = pmfpPolicyNum
        !mintTokenNameBase = pmfpTokenNameBase
        !mintDifTokenNameCount = pmfpDiifTokenNameCount
        !mintAmount = pmfpAmount
    ---------------------
        !policy_MintFT = OnChain.policyFT mintPolicyNum
    ---------------------
        !mintFree_CS = OffChainHelpers.getCurSymbolOfPolicy policy_MintFT
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
        !mintFree_TNS =
            if mintDifTokenNameCount > 1
                then do
                    [LedgerApiV2.TokenName (mintTokenNameBase <> OnChainHelpers.intToBBS num) | num <- [1 .. mintDifTokenNameCount] :: [Integer]]
                else [LedgerApiV2.TokenName mintTokenNameBase]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "mintFree TNS: %s" (P.show mintFree_TNS)
    ---------------------
    let !valueFor_Mint_MintFT =
            foldl
                (<>)
                (LedgerAda.lovelaceValueOf 0)
                ( [ let !mintFree_AC = LedgerValue.AssetClass (mintFree_CS, mintFree_TN')
                    in  LedgerValue.assetClassValue mintFree_AC mintAmount
                    | mintFree_TN' <- mintFree_TNS
                  ]
                )

        !lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtUser
                P.<> LedgerConstraints.plutusV2MintingPolicy policy_MintFT
        !tx =
            LedgerConstraints.mustMintValue valueFor_Mint_MintFT
                P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                P.<> LedgerConstraints.mustBeSignedBy userPPKH
    ---------------------
    let !eval_MintingPolicies = [  (mintFree_CS, policy_MintFT)]
        !eval_Validators = [ ]
        !eval_GetDatums = [ ]
    ---------------------
    OffChainEval.evalAndSubmitTx nameEndPoint eval_MintingPolicies eval_Validators eval_GetDatums lookupsTx tx

--------------------------------------------------------------------------------2

endPointMintNFT :: T.PABMintNFTParams -> PlutusContract.Contract w s DataText.Text ()
endPointMintNFT T.PABMintNFTParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Mint NFT"
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
    let !mintTokenNameBase = pmnpTokenNameBase
        !mintDifTokenNameCount = pmnpDiifTokenNameCount
        !mintAmount = pmnpAmount
    ---------------------
        !uTxOforMinNFT = P.head (DataMap.toList uTxOsAtUser)
        !policy_MintNFT = OnChain.policyNFT (fst uTxOforMinNFT)
    ---------------------
        !mintNFT_CS = OffChainHelpers.getCurSymbolOfPolicy policy_MintNFT
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
        !mintNFT_TNS =
            if mintDifTokenNameCount > 1
                then do
                    [LedgerApiV2.TokenName (mintTokenNameBase <> OnChainHelpers.intToBBS num) | num <- [1 .. mintDifTokenNameCount] :: [Integer]]
                else [LedgerApiV2.TokenName mintTokenNameBase]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "mintFree TNS: %s" (P.show mintNFT_TNS)
    ---------------------
    let !valueFor_Mint_NFT =
            foldl
                (<>)
                (LedgerAda.lovelaceValueOf 0)
                ( [ let !mintNFT_AC = LedgerValue.AssetClass (mintNFT_CS, mintNFT_TN')
                    in  LedgerValue.assetClassValue mintNFT_AC mintAmount
                    | mintNFT_TN' <- mintNFT_TNS
                  ]
                )

        !lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtUser
                P.<> LedgerConstraints.plutusV2MintingPolicy policy_MintNFT
        !tx =
            LedgerConstraints.mustMintValue valueFor_Mint_NFT
                P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                P.<> LedgerConstraints.mustBeSignedBy userPPKH
    ---------------------
    let !eval_MintingPolicies = [  (mintNFT_CS, policy_MintNFT)]
        !eval_Validators = [ ]
        !eval_GetDatums = [ ]
    ---------------------
    OffChainEval.evalAndSubmitTx nameEndPoint eval_MintingPolicies eval_Validators eval_GetDatums lookupsTx tx

--------------------------------------------------------------------------------2

endPointMintFundTokens :: T.PABMintFundTokensParams -> PlutusContract.Contract w s DataText.Text ()
endPointMintFundTokens T.PABMintFundTokensParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let nameEndPoint = "Mint Fund Tokens"
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
    let 
        -- !protocolPABParams = pmftpProtocolPABParams
        !fundPABParams = pmftpFundPABParams
    ---------------------
    let !investUnitValidator_Address = T.fppInvestUnitValidator_Address fundPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let !mintAmount = pmftpAmount
    ---------------------
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    ---------------------
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    ---------------------
    let !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
        !investUnit_In = InvestUnitT.iudInvestUnit investUnitDatum_In
        !investUnitTokens_In = T.iuValues investUnit_In
    ---------------------
    let listPolicies = [OnChain.policyFT num | num <- [0..10]]
    let listPoliciesAndCS = [(OffChainHelpers.getCurSymbolOfPolicy pol, pol) | pol <- listPolicies]
    ---------------------
    let getPolicy cs = case find (\(cs', _) -> cs == cs') listPoliciesAndCS of
            Just (_, pol) -> return pol
            Nothing -> do PlutusContract.throwError "policy not found"
    ---------------------
    policies_MintFundTokens <- Monad.foldM (\acc (cs, tn, am) -> do
                    pol <- getPolicy cs
                    return $ (cs, tn, am, pol) : acc
                 ) [] investUnitTokens_In
    ---------------------
    let valueFor_Mint_MintFundTokens = foldl (P.<>) (LedgerAda.lovelaceValueOf 0) [LedgerValue.assetClassValue (LedgerValue.AssetClass (cs, tn)) (am*mintAmount) | (cs, tn, am, _) <- policies_MintFundTokens]
    ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
        !lookupsTx =
            LedgerConstraints.unspentOutputs uTxOsAtUser P.<>
                foldl (P.<>) (LedgerConstraints.unspentOutputs DataMap.empty) [LedgerConstraints.plutusV2MintingPolicy policy_MintFundTokens | (_, _, _, policy_MintFundTokens) <- policies_MintFundTokens]

        !tx =
            LedgerConstraints.mustMintValue valueFor_Mint_MintFundTokens
                P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                P.<> LedgerConstraints.mustBeSignedBy userPPKH
    ---------------------
    let !eval_MintingPolicies = [ (cs, policy_MintFundTokens) | (cs, _, _, policy_MintFundTokens) <- policies_MintFundTokens]
        !eval_Validators = [ ]
        !eval_GetDatums = [ ]
    ---------------------
    OffChainEval.evalAndSubmitTx nameEndPoint eval_MintingPolicies eval_Validators eval_GetDatums lookupsTx tx

--------------------------------------------------------------------------------2
