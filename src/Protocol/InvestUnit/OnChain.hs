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
module Protocol.InvestUnit.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2 
import qualified PlutusTx
import           PlutusTx.Prelude
import PlutusTx (CompiledCode)
import qualified Ledger
import qualified Ledger.Value as LedgerValue

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2
import qualified Protocol.Constants    as T
import qualified Protocol.Fund.Holding.Types       as FundHoldingT
import qualified Protocol.InvestUnit.Types       as T
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator T.ValidatorParams{..} !datumRaw !redRaw !ctxRaw =  
    if 
        traceIfFalse "" useThisToMakeScriptUnique 
        && 
        case redeemer of
            (T.ValidatorRedeemerReIndexing T.ValidatorRedeemerReIndexingType {..}) ->
                   traceIfFalse "not isCorrect_Oracle_Signature" isCorrect_Oracle_Signature
                && traceIfFalse "not isCorrect_Oracle_InRangeTime" isCorrect_Oracle_InRangeTime
                && traceIfFalse "not isCorrect_Exchange_WithSamePriceADA" isCorrect_Exchange_WithSamePriceADA
                && traceIfFalse "not isCorrect_Output_InvestUnitDatum_WithTokensExchanged" isCorrect_Output_InvestUnitDatum_WithTokensExchanged
                && traceIfFalse "not isCorrect_Output_InvestDatum_Value_NotChanged" isCorrect_Output_InvestDatum_Value_NotChanged
                && traceIfFalse "not isCorrect_Redeemer_FundHoldingDatum" isCorrect_Redeemer_FundHoldingDatum
                where
                ------------------
                    !investUnitTokensToAdd = riuriTokensToAdd 
                    !investUnitTokensToRemove = riuriTokensToRemove 
                ------------------
                    !investUnit_In = T.iudInvestUnit investUnitDatum_In
                    !investUnitTokens_In = T.iuValues investUnit_In
                ------------------
                    !tokensToAdd = T.iuValues investUnitTokensToAdd
                    !tokensToRemove = T.iuValues investUnitTokensToRemove
                ------------------
                    !investUnit_Out' = OnChainHelpers.flattenValueAdd investUnitTokens_In tokensToAdd
                    !investUnit_Out = OnChainHelpers.flattenValueSub investUnit_Out' tokensToRemove
                    ------------------
                    !valueFor_InvestUnitDatum_Control_NotChanged = valueOf_InvestUnitDatum_In
                ------------------
                    !investUnitDatum_Control = T.mkInvestUnitDatumType (T.iudFundPolicy_CS investUnitDatum_In) (T.InvestUnit investUnit_Out) (T.iudMinADA investUnitDatum_In)
                ------------------
                    !tokensPricesADA =  T.iuValues $ T.oridTokensPriceADA riuriOracleReIdx_Data
                ------------------
                    !priceADAOf_TokensForTokensToAdd = sum (findPriceADA <$> tokensToAdd)
                -------------------
                    !priceADAOf_TokensForTokensToRemove = sum (findPriceADA <$> tokensToRemove)
                ------------------
                    findPriceADA :: T.InvestUnitToken -> Integer
                    findPriceADA (cs, tn, amt) = 
                        case find (\(cs', tn', _) -> cs' == cs && tn' == tn) tokensPricesADA of
                        Nothing            -> traceError "No price found for token"
                        Just (_, _, price) -> amt * price
                -------------------
                    isCorrect_Oracle_Signature :: Bool
                    isCorrect_Oracle_Signature = 
                        let 
                            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                        ------------------
                            !inputsRef_TxOuts_And_ProtocolDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType ctx protocolID_AC ProtocolT.getProtocolDatumType
                            !inputRef_TxOut_And_ProtocolDatum = case inputsRef_TxOuts_And_ProtocolDatums of
                                [x] -> x
                                _   -> traceError "expected exactly one Protocol input ref"
                        ------------------
                            !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                        ------------------
                            !oraclePaymentPubKey = ProtocolT.pdOraclePaymentPubKey protocolDatum_In
                        ------------------
                            !priceData = OnChainHelpers.oracleReIdxDataToBBS riuriOracleReIdx_Data
                        ------------------
                        in  case OnChainHelpers.checkSignature oraclePaymentPubKey priceData riuriOracleSignature of
                                Left _ -> False
                                Right _ -> True
                ------------------
                    isCorrect_Oracle_InRangeTime :: Bool
                    isCorrect_Oracle_InRangeTime =  
                        let
                            validRange = LedgerApiV2.txInfoValidRange info
                            newLowerLimitValue :: LedgerApiV2.POSIXTime
                            newLowerLimitValue = case Ledger.ivFrom validRange of
                                Ledger.LowerBound (Ledger.Finite a) True -> a - T.oracleData_Valid_Time
                                _-> traceError "Interval has no lower bound"
                            
                            newInterval = Ledger.Interval (Ledger.LowerBound (Ledger.Finite newLowerLimitValue) True) (Ledger.ivTo validRange )
                            -- TODO: la valides de la transaccion hay que ponerla en 3 minutos
                        in
                            T.oridTime riuriOracleReIdx_Data `Ledger.member` newInterval
                ------------------
                    isCorrect_Exchange_WithSamePriceADA :: Bool
                    isCorrect_Exchange_WithSamePriceADA =  priceADAOf_TokensForTokensToAdd == priceADAOf_TokensForTokensToRemove
                ------------------
                    isCorrect_Output_InvestUnitDatum_WithTokensExchanged :: Bool
                    isCorrect_Output_InvestUnitDatum_WithTokensExchanged =
                        let 
                            !investUnitDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_InvestUnitDatum
                        in  investUnitDatum_Out `OnChainHelpers.isUnsafeEqDatums` investUnitDatum_Control
                ------------------
                    isCorrect_Output_InvestDatum_Value_NotChanged :: Bool
                    isCorrect_Output_InvestDatum_Value_NotChanged =
                        let !valueOf_InvestUnitDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_InvestUnitDatum
                        in  valueOf_InvestUnitDatum_Out `OnChainHelpers.isEqValue` valueFor_InvestUnitDatum_Control_NotChanged
                -------------------
                    isCorrect_Redeemer_FundHoldingDatum :: Bool
                    isCorrect_Redeemer_FundHoldingDatum  = 
                        let 
                        ------------------
                            !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                        ------------------
                            !inputs_TxOuts_And_FundDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_InputsRef_By_AC @FundT.ValidatorDatum @FundT.FundDatumType ctx fundID_AC FundT.getFundDatumType
                            !input_TxOut_And_FundDatum = case inputs_TxOuts_And_FundDatums of
                                [x] -> x
                                _   -> traceError "expected exactly one Fund input ref"
                        ------------------
                            !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
                        ------------------
                            !fundHoldingID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_In
                        ------------------
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
                                            Just (FundHoldingT.ValidatorRedeemerReIndexing FundHoldingT.ValidatorRedeemerReIndexingType {..}) -> 
                                                riuriTokensToAdd == rriTokensToAdd && riuriTokensToRemove == rriTokensToRemove
                                            _ -> False    
                ------------------
        then ()
        else error ()     
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
                !investUnitDatum_In = T.getInvestUnitDatumType datum
             ------------------
                !fundPolicy_CS = T.iudFundPolicy_CS investUnitDatum_In
            ------------------
                !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
            ------------------
                !valueOf_InvestUnitDatum_In = LedgerApiV2.txOutValue input_Current_TxOut
            ------------------
                !outputs_Own_TxOuts = OnChainHelpers.getUnsafe_Own_Outputs_TxOuts ctx
            ------------------
                !outputs_Own_TxOuts_And_InvestUnitDatums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_AC @T.ValidatorDatum @T.InvestUnitDatumType outputs_Own_TxOuts ctx investUnitID_AC T.getInvestUnitDatumType
                !output_Own_TxOut_And_InvestUnitDatum = case outputs_Own_TxOuts_And_InvestUnitDatums of
                    [x] -> x
                    _   -> traceError "expected exactly one InvestUnit output"
            ------------------
                

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
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS = mkValidator params
    where
        params = T.ValidatorParams
            { 
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
            }

validatorCode :: CompiledCode ( BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$( PlutusTx.compile [|| mkWrappedValidator ||])


--------------------------------------------------------------------------------2
