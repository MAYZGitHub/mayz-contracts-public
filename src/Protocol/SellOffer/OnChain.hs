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
module Protocol.SellOffer.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import           Data.Aeson                (Value (Bool))
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Ledger.Ada as LedgerAda
import qualified Ledger
--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants         as T
import qualified Generic.Types             as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.SellOffer.Types  as T
import qualified Protocol.Types            as T
import qualified Protocol.Constants as T
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.OnChainHelpers as OnChainHelpers

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
        !sellOffer_Validator_Hash = ppSellOffer_Validator_Hash
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
     ------------------
        !sellOfferPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
    ------------------
        !valueFor_Mint_SellOffer_ID = LedgerValue.assetClassValue sellOffer_ID_AC 1
    ------------------
       -- !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !sellOffer_ID_AC = LedgerValue.AssetClass (sellOfferPolicyID_CS, T.sellOfferID_TN)
    ------------------
    in  if traceIfFalse "" useThisToMakeScriptUnique
            &&
            case redeemer of
                T.PolicyRedeemerCreateSellOfferID _ -> 
                    -- que se mintee ID de Sell Order, con esta poliza, 1 unidad, con nombre de token que venga en datum del protocolo
                    -- que se cree datum correcto (parametros dentro de rangos permitidos, totales en cero, seller que firma la tx)
                    -- que vaya a la direccion del contrato correcta. La direccion puede estar en el datum del protocolo
                    traceIfFalse "not isMintingSellOfferID" isMintingSellOfferID &&
                    traceIfFalse "not isCorrect_Output_Address" isCorrect_Output_Address &&
                    traceIfFalse "not isCorrect_Output_SellOffer_Datum" isCorrect_Output_SellOffer_Datum &&
                    traceIfFalse "not isCorrect_Output_SellOffer_Value" isCorrect_Output_SellOffer_Value 
                    ---------------------
                    where
                    ------------------
                        !outputs_TxOuts_And_SellOffer_Datums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_AC @T.ValidatorDatum @T.SellOffer_DatumType ctx sellOffer_ID_AC T.getSellOffer_DatumType
                        !output_TxOut_And_SellOffer_Datum = case outputs_TxOuts_And_SellOffer_Datums of
                            [x] -> x
                            _   -> traceError "expected exactly one SellOffer output"
                    ---------------------
                        !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_SellOffer_Datum
                    ---------------------
                        isMintingSellOfferID :: Bool
                        isMintingSellOfferID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_SellOffer_ID
                    -----------------
                        isCorrect_Output_Address :: Bool
                        isCorrect_Output_Address =
                            let
                            ------------------
                                !sellOffer_Datum_Address = OnChainHelpers.getAddress_In_TxOut_And_Datum output_TxOut_And_SellOffer_Datum
                            ------------------
                                !sellOffer_Datum_Address_Hash = OnChainHelpers.getUnsafeScriptHash_In_Address sellOffer_Datum_Address
                            ------------------
                            in sellOffer_Datum_Address_Hash == sellOffer_Validator_Hash
                    ------------------
                        isCorrect_Output_SellOffer_Datum :: Bool
                        isCorrect_Output_SellOffer_Datum = 
                            -- TODO controlar que sodAskedCommission_Rate_InBPx1e3 este dentro del rango permitido en el protocolo
                            let !sellOffer_Datum_Out_Control = 
                                    T.mkSellOffer_DatumType 
                                        sellOfferPolicyID_CS
                                        (T.sodFundPolicy_CS sellOffer_Datum_Out)
                                        (T.sodSellerPaymentPKH sellOffer_Datum_Out)
                                        (T.sodSellerStakePKH sellOffer_Datum_Out)
                                        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_Out)
                                        (T.sodAmount_FT_Available  sellOffer_Datum_Out)
                                        (T.sodAmount_ADA_Available sellOffer_Datum_Out)
                                        0
                                        0
                                        T.sellOffer_Status_Open
                                        (T.sodMinADA sellOffer_Datum_Out)
                            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
                    ------------------
                        isCorrect_Output_SellOffer_Value :: Bool
                        isCorrect_Output_SellOffer_Value = 
                            let 
                            ---------------------
                                !fundFT_TN_AC = LedgerValue.AssetClass (T.sodFundPolicy_CS sellOffer_Datum_Out, T.fundFT_TN)
                            ---------------------
                                !amount_FT_Available_For_SellOffer_Datum = T.sodAmount_FT_Available  sellOffer_Datum_Out
                                !amount_ADA_Available_For_SellOffer_Datum = T.sodAmount_ADA_Available sellOffer_Datum_Out
                            ---------------------
                                !value_Amount_FT_Available_For_SellOffer_Datum = LedgerValue.assetClassValue fundFT_TN_AC amount_FT_Available_For_SellOffer_Datum
                                !value_Amount_ADA_Available_For_SellOffer_Datum = LedgerAda.lovelaceValueOf amount_ADA_Available_For_SellOffer_Datum
                            ---------------------
                                !minADA_For_SellOffer_Datum = T.sodMinADA sellOffer_Datum_Out
                                !value_MinADA_For_SellOffer_Datum = LedgerAda.lovelaceValueOf minADA_For_SellOffer_Datum
                            ---------------------
                                !valueFor_SellOffer_Datum_Out_Control = valueFor_Mint_SellOffer_ID <> value_MinADA_For_SellOffer_Datum <> value_Amount_FT_Available_For_SellOffer_Datum <> value_Amount_ADA_Available_For_SellOffer_Datum
                            ---------------------
                                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_SellOffer_Datum
                            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Datum_Out_Control
                    ------------------
                T.PolicyRedeemerDeleteSellOfferID _ -> 
                    -- que se queme ID del Sell Order, 1 unidad. Creo que con esto es suficiente.
                    -- que se este ejecutando validador correcto. No seria necesario. Si se quema es por que sale de algun lado.
                    ---------------------
                        traceIfFalse "not isBurningSellOfferID" isBurningSellOfferID
                    ---------------------
                    where
                    ------------------
                        !valueFor_Burn_SellOffer_ID = LedgerValue.assetClassValue sellOffer_ID_AC (negate 1)
                    ---------------------
                        isBurningSellOfferID :: Bool
                        isBurningSellOfferID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_SellOffer_ID
                    -----------------
            then ()
            else error ()


--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_StatusChanged #-}
mkUpdated_SellOffer_Datum_With_StatusChanged :: T.SellOffer_DatumType -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_StatusChanged !sellOffer_Datum_In !newStatus =
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In)
        (T.sodAmount_FT_Available sellOffer_Datum_In)
        (T.sodAmount_ADA_Available sellOffer_Datum_In)
        (T.sodTotal_FT_Earned sellOffer_Datum_In)
        (T.sodTotal_ADA_Earned sellOffer_Datum_In)
        newStatus
        (T.sodMinADA sellOffer_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_CommissionChanged #-}
mkUpdated_SellOffer_Datum_With_CommissionChanged :: T.SellOffer_DatumType -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_CommissionChanged !sellOffer_Datum_In !newCommissionRate =
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        newCommissionRate
        (T.sodAmount_FT_Available sellOffer_Datum_In)
        (T.sodAmount_ADA_Available sellOffer_Datum_In)
        (T.sodTotal_FT_Earned sellOffer_Datum_In)
        (T.sodTotal_ADA_Earned sellOffer_Datum_In)
        (T.sodOrder_Status sellOffer_Datum_In)
        (T.sodMinADA sellOffer_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_MinADAChanged #-}
mkUpdated_SellOffer_Datum_With_MinADAChanged :: T.SellOffer_DatumType -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_MinADAChanged !sellOffer_Datum_In !newMinADA =
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In)
        (T.sodAmount_FT_Available sellOffer_Datum_In)
        (T.sodAmount_ADA_Available sellOffer_Datum_In)
        (T.sodTotal_FT_Earned sellOffer_Datum_In)
        (T.sodTotal_ADA_Earned sellOffer_Datum_In)
        (T.sodOrder_Status sellOffer_Datum_In)
        newMinADA

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_Deposit #-}
mkUpdated_SellOffer_Datum_With_Deposit :: T.SellOffer_DatumType -> Integer -> Integer ->  T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_Deposit !sellOffer_Datum_In !newDeposit_FT !newDeposit_ADA =
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In)
        (T.sodAmount_FT_Available sellOffer_Datum_In + newDeposit_FT)
        (T.sodAmount_ADA_Available sellOffer_Datum_In + newDeposit_ADA)
        (T.sodTotal_FT_Earned sellOffer_Datum_In)
        (T.sodTotal_ADA_Earned sellOffer_Datum_In)
        (T.sodOrder_Status sellOffer_Datum_In)
        (T.sodMinADA sellOffer_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_Withdraw #-}
mkUpdated_SellOffer_Datum_With_Withdraw :: T.SellOffer_DatumType -> Integer -> Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_Withdraw !sellOffer_Datum_In !newWithdraw_FT !newWithdraw_ADA =
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In)
        (T.sodAmount_FT_Available sellOffer_Datum_In - newWithdraw_FT)
        (T.sodAmount_ADA_Available sellOffer_Datum_In - newWithdraw_ADA)
        (T.sodTotal_FT_Earned sellOffer_Datum_In )
        (T.sodTotal_ADA_Earned sellOffer_Datum_In )
        (T.sodOrder_Status sellOffer_Datum_In)
        (T.sodMinADA sellOffer_Datum_In)

     --------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_SwapFTxADA #-}
mkUpdated_SellOffer_Datum_With_SwapFTxADA :: T.SellOffer_DatumType -> Integer ->Integer -> Integer ->T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_SwapFTxADA !sellOffer_Datum_In !amount_FT !amount_ADA !commission_ADA=
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In)
        (T.sodAmount_FT_Available sellOffer_Datum_In + amount_FT)
        (T.sodAmount_ADA_Available sellOffer_Datum_In - (amount_ADA - commission_ADA) )
        (T.sodTotal_FT_Earned sellOffer_Datum_In)
        (T.sodTotal_ADA_Earned sellOffer_Datum_In + commission_ADA)
        (T.sodOrder_Status sellOffer_Datum_In)
        (T.sodMinADA sellOffer_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SellOffer_Datum_With_SwapADAxFT #-}
mkUpdated_SellOffer_Datum_With_SwapADAxFT :: T.SellOffer_DatumType -> Integer -> Integer ->Integer -> T.SellOffer_DatumType
mkUpdated_SellOffer_Datum_With_SwapADAxFT !sellOffer_Datum_In !amount_ADA !amount_FT !commission_FT =
    T.mkSellOffer_DatumType
       (T.sodSellOfferPolicyID_CS sellOffer_Datum_In)
       (T.sodFundPolicy_CS sellOffer_Datum_In)
        (T.sodSellerPaymentPKH sellOffer_Datum_In)
        (T.sodSellerStakePKH sellOffer_Datum_In)
        (T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In)
        (T.sodAmount_FT_Available sellOffer_Datum_In - (amount_FT - commission_FT))
        (T.sodAmount_ADA_Available sellOffer_Datum_In + amount_ADA)
        (T.sodTotal_FT_Earned sellOffer_Datum_In + commission_FT)
        (T.sodTotal_ADA_Earned sellOffer_Datum_In)
        (T.sodOrder_Status sellOffer_Datum_In)
        (T.sodMinADA sellOffer_Datum_In)
           
--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator T.ValidatorParams {..} !datumRaw !redRaw !ctxRaw =
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
        !protocolPolicyID_CS = vpProtocolPolicyID_CS
        -- !sellOfferPolicyID_CS = vpSellOfferPolicyID_CS
    ------------------
        !input_Current_TxOut = OnChainHelpers.getUnsafe_Current_Input_TxOut ctx
    ------------------
        !sellOffer_Datum_In = T.getSellOffer_DatumType datum
    ------------------
        !sellOfferPolicyID_CS = T.sodSellOfferPolicyID_CS sellOffer_Datum_In
    ------------------
        !fundPolicy_CS = T.sodFundPolicy_CS sellOffer_Datum_In
        !fundFT_TN_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundFT_TN)
    ---------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- No hace falta uso las dos variables en la poliza
    ------------------
        redeemerUpdateOrderStatus = 1
        redeemerUpdateAskedCommissionRate = 2
        redeemerUpdateMinADA = 3
        redeemerDeposit = 4
        redeemerWithdraw = 5
        redeemerSwapFTxADA = 6
        redeemerSwapADAxFT = 7
        redeemerDelete = 8
    ------------------
        getRedeemerType :: Integer
        getRedeemerType = case redeemer of
            (T.ValidatorRedeemerUpdateOrderStatus _)    -> redeemerUpdateOrderStatus
            (T.ValidatorRedeemerUpdateAskedCommissionRate _) -> redeemerUpdateAskedCommissionRate
            (T.ValidatorRedeemerUpdateMinADA _)         -> redeemerUpdateMinADA
            (T.ValidatorRedeemerDeposit _)               -> redeemerDeposit
            (T.ValidatorRedeemerWithdraw _)             -> redeemerWithdraw
            (T.ValidatorRedeemerSwapFTxADA _)           -> redeemerSwapFTxADA
            (T.ValidatorRedeemerSwapADAxFT _)           -> redeemerSwapADAxFT
            (T.ValidatorRedeemerDelete _)               -> redeemerDelete
    ------------------
        validateRedeemer :: Integer -> Bool
        validateRedeemer redeemerType
            | redeemerType == redeemerUpdateOrderStatus = validateAdminAction (T.sodSellerPaymentPKH sellOffer_Datum_In) && validateUpdateOrderStatus redeemer
            | redeemerType == redeemerUpdateAskedCommissionRate  = validateAdminAction (T.sodSellerPaymentPKH sellOffer_Datum_In) && validatUpdateAskedCommissionRate redeemer
            | redeemerType == redeemerUpdateMinADA = validateAdminAction (T.sodSellerPaymentPKH sellOffer_Datum_In) && validateUpdateMinADA redeemer
            | redeemerType == redeemerDeposit = validateAdminAction (T.sodSellerPaymentPKH sellOffer_Datum_In) && validateDeposit redeemer
            | redeemerType == redeemerWithdraw = validateAdminAction (T.sodSellerPaymentPKH sellOffer_Datum_In) && validateWithdraw redeemer
            | redeemerType == redeemerSwapFTxADA = validateSwapFTxADA redeemer
            | redeemerType == redeemerSwapADAxFT = validateSwapADAxFT redeemer
            | redeemerType == redeemerDelete = validateAdminAction (T.sodSellerPaymentPKH sellOffer_Datum_In) && validateDelete
            | otherwise = False
    ------------------
        validateAdminAction :: T.WalletPaymentPKH -> Bool
        validateAdminAction !admin =
            traceIfFalse "not isSignedByAny admin" (LedgerContextsV2.txSignedBy info admin)
            && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
    ------------------
        validateUpdateOrderStatus :: T.ValidatorRedeemer  -> Bool
        validateUpdateOrderStatus (T.ValidatorRedeemerUpdateOrderStatus T.ValidatorRedeemerUpdateOrderStatusType{..})  = 
        ---- change status to open or close. Only admin can do it
        -- check that there is one input and one output in this contract
        -- check datum update with new status 
        -- check value not changed
               traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_StatusChanged" (isCorrect_Output_SellOffer_Datum_With_StatusChanged ruosNewStatus)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_NotChanged" isCorrect_Output_SellOffer_Value_NotChanged 
        validateUpdateOrderStatus _   = False
    ------------------
        validatUpdateAskedCommissionRate :: T.ValidatorRedeemer  ->  Bool
        validatUpdateAskedCommissionRate  (T.ValidatorRedeemerUpdateAskedCommissionRate T.ValidatorRedeemerUpdateAskedCommissionRateType{..})  = 
        ---- change commission rate. Only admin can do it. Must be in the range of protocol datum commissionRateForSellOffersRange
        -- check that there is one input and one output in this contract
        -- check datum update with new commissions rate. Must be in the range of protocol datum commissionRateForSellOffersRange
        -- check value not changed
               traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_CommissionChanged" (isCorrect_Output_SellOffer_Datum_With_CommissionChanged rucrNewCommissionRate)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_NotChanged" isCorrect_Output_SellOffer_Value_NotChanged 
        validatUpdateAskedCommissionRate _   = False
    ------------------
        validateUpdateMinADA :: T.ValidatorRedeemer  ->  Bool
        validateUpdateMinADA (T.ValidatorRedeemerUpdateMinADA T.ValidatorRedeemerUpdateMinADAType{..})  = 
        ---- change min ada value. Only admin can do it
        -- check that there is one input and one output in this contract
        -- check datum update with new minAda 
        -- check value changed ADA
               traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_MinADAChanged" (isCorrect_Output_SellOffer_Datum_With_MinADAChanged rumaNewMinADA)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_MinADAChanged" (isCorrect_Output_SellOffer_Value_With_MinADAChanged rumaNewMinADA)
        validateUpdateMinADA _   = False
     ------------------
        validateDeposit :: T.ValidatorRedeemer  ->  Bool
        validateDeposit (T.ValidatorRedeemerDeposit T.ValidatorRedeemerDepositType{..})  = 
        ---- add some FT or ADA. Only admin can do it.
        -- check that there is one input and one output in this contract
        -- check datum update with deposit ... me parece que no hay cambios que se hagan en el datum en esta tx
        -- check value changed with deposit
               traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_Deposit" (isCorrect_Output_SellOffer_Datum_With_Deposit rdNewDeposit_FT rdNewDeposit_ADA)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_Deposit" (isCorrect_Output_SellOffer_Value_With_Deposit rdNewDeposit_FT rdNewDeposit_ADA)
        validateDeposit _   = False
    ------------------
        validateWithdraw :: T.ValidatorRedeemer  ->  Bool
        validateWithdraw (T.ValidatorRedeemerWithdraw T.ValidatorRedeemerWithdrawType{..})  = 
        ---- get back some FT or ADA. Only admin can do it.
        -- check that there is one input and one output in this contract
        -- check datum update with withdraw ... me parece que no hay cambios que se hagan en el datum en esta tx
        -- check value changed with withdraw
            traceIfFalse    "not isCorrect_Output_SellOffer_Datum_With_Withdraw" (isCorrect_Output_SellOffer_Datum_With_Withdraw rwNewWithdraw_FT rwNewWithdraw_ADA)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_Withdraw" (isCorrect_Output_SellOffer_Value_With_Withdraw rwNewWithdraw_FT rwNewWithdraw_ADA)
        validateWithdraw _   = False
    ------------------
        validateSwapFTxADA :: T.ValidatorRedeemer  ->  Bool
        validateSwapFTxADA (T.ValidatorRedeemerSwapFTxADA T.ValidatorRedeemerSwapFTxADAType{..})  = 
        ---- if order is open, user give FT and get ADA. Use a price for conversion provided by oracle. Must check signatura and validity time
        -- check that there is one input and one output in this contract
        -- check datum update with swap. Totals calculated
        -- check commissions
        -- check value changed FT and ADA
        -- check price, validity time and signature
               traceIfFalse "not isOrderOpen" isOrderOpen
            && traceIfFalse "not isCorrect_Oracle_Signature" (isCorrect_Oracle_Signature rsfxaOracle_Data rsfxaOracle_Signature)
            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (isCorrect_Oracle_InRangeTime rsfxaOracle_Data)
            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsfxaOracle_Data rsfxaAmount_FT rsfxaAmount_ADA)
            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsfxaAmount_ADA rsfxaCommission_ADA)
            && traceIfFalse "not isAmount_ADA_Available" (isAmount_ADA_Available (rsfxaAmount_ADA-rsfxaAmount_ADA))
            && traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_SwapFTxADA" (isCorrect_Output_SellOffer_Datum_With_SwapFTxADA rsfxaAmount_FT rsfxaAmount_ADA rsfxaCommission_ADA)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_SwapFTxADA" (isCorrect_Output_SellOffer_Value_With_SwapFTxADA rsfxaAmount_FT rsfxaAmount_ADA rsfxaCommission_ADA)
        validateSwapFTxADA _   = False
    ------------------
        validateSwapADAxFT :: T.ValidatorRedeemer  ->  Bool
        validateSwapADAxFT (T.ValidatorRedeemerSwapADAxFT T.ValidatorRedeemerSwapADAxFTType{..})  = 
        ---- if order is open, user give ADA and get FT. Use a price for conversion provided by oracle. Must check signatura and validity time
        -- check that there is one input and one output in this contract
        -- check datum update with swap. Totals calculated
        -- check commissions
        -- check value changed FT and ADA
        -- check price, validity time and signature
               traceIfFalse "not isOrderOpen" isOrderOpen
            && traceIfFalse "not isCorrect_Oracle_Signature" (isCorrect_Oracle_Signature rsaxfOracle_Data rsaxfOracle_Signature)
            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (isCorrect_Oracle_InRangeTime rsaxfOracle_Data )
            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsaxfOracle_Data rsaxfAmount_FT rsaxfAmount_ADA )
            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsaxfAmount_FT rsaxfCommission_FT)
            && traceIfFalse "not isAmount_FT_Available" (isAmount_FT_Available (rsaxfAmount_FT-rsaxfCommission_FT))
            && traceIfFalse "not isCorrect_Output_SellOffer_Datum_With_SwapADAxFT" (isCorrect_Output_SellOffer_Datum_With_SwapADAxFT rsaxfAmount_ADA rsaxfAmount_FT rsaxfCommission_FT)
            && traceIfFalse "not isCorrect_Output_SellOffer_Value_With_SwapADAxFT" (isCorrect_Output_SellOffer_Value_With_SwapADAxFT rsaxfAmount_ADA rsaxfAmount_FT rsaxfCommission_FT)
        validateSwapADAxFT _   = False
    ------------------
        validateDelete :: Bool
        validateDelete = traceIfFalse "not isBurningSellOfferID" isBurningSellOfferID
        ---- get back all FT and ADA and delete datum utxo. Burn order ID. only admin can do it
        -- check that there is one input and zero output in this contract
        -- check that ID is burning
     ------------------
        isOrderOpen :: Bool 
        isOrderOpen = T.sodOrder_Status sellOffer_Datum_In == T.sellOffer_Status_Open
     ------------------
        getOutput_Own_TxOut_And_SellOffer_Datum :: (LedgerApiV2.TxOut, T.SellOffer_DatumType)
        getOutput_Own_TxOut_And_SellOffer_Datum =
            let
                !outputs_Own_TxOuts = OnChainHelpers.getUnsafe_Own_Outputs_TxOuts ctx
            ------------------
                !outputs_Own_TxOuts_And_SellOffer_Datums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.SellOffer_DatumType outputs_Own_TxOuts ctx sellOfferPolicyID_CS T.getSellOffer_DatumType
                !output_Own_TxOut_And_SellOffer_Datum = case outputs_Own_TxOuts_And_SellOffer_Datums of
                    [x] -> x
                    _   -> traceError "expected exactly one SellOffer output"
            ------------------
            in output_Own_TxOut_And_SellOffer_Datum
    ------------------
        isCorrect_Output_SellOffer_Datum_With_StatusChanged:: Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_StatusChanged !newStatus =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_StatusChanged sellOffer_Datum_In newStatus
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Datum_With_CommissionChanged:: Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_CommissionChanged !newCommissionRate =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_CommissionChanged sellOffer_Datum_In newCommissionRate
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Datum_With_MinADAChanged:: Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_MinADAChanged !newMinADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_MinADAChanged sellOffer_Datum_In newMinADA
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Datum_With_Deposit:: Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_Deposit !newDeposit_FT !newDeposit_ADA  =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_Deposit sellOffer_Datum_In newDeposit_FT newDeposit_ADA
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Datum_With_Withdraw:: Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_Withdraw !newWithdraw_FT !newWithdraw_ADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_Withdraw sellOffer_Datum_In newWithdraw_FT newWithdraw_ADA
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Datum_With_SwapFTxADA:: Integer -> Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_SwapFTxADA !amount_FT !amount_ADA !commission_ADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_SwapFTxADA sellOffer_Datum_In amount_FT amount_ADA commission_ADA
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Datum_With_SwapADAxFT:: Integer -> Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Datum_With_SwapADAxFT !amount_ADA !amount_FT !commission_FT =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !sellOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !sellOffer_Datum_Out_Control = mkUpdated_SellOffer_Datum_With_SwapADAxFT sellOffer_Datum_In amount_ADA amount_FT commission_FT
            in  sellOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` sellOffer_Datum_Out_Control
    ------------------
        isCorrect_Output_SellOffer_Value_NotChanged :: Bool
        isCorrect_Output_SellOffer_Value_NotChanged =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
                !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut
            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
     ----------------
        isCorrect_Output_SellOffer_Value_With_MinADAChanged ::  Integer -> Bool
        isCorrect_Output_SellOffer_Value_With_MinADAChanged !newMinADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
            ------------------
                !value_MinADA_SellOffer_Datum_In = LedgerAda.lovelaceValueOf (T.sodMinADA sellOffer_Datum_In)
                !value_MinADA_SellOffer_Datum_Out = LedgerAda.lovelaceValueOf newMinADA
            ------------------
                !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> negate value_MinADA_SellOffer_Datum_In <> value_MinADA_SellOffer_Datum_Out
            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
    ----------------
        isCorrect_Output_SellOffer_Value_With_Deposit ::  Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Value_With_Deposit !newDeposit_FT !newDeposit_ADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
            ------------------
                !value_Deposit_FT = LedgerValue.assetClassValue fundFT_TN_AC newDeposit_FT
                !value_Deposit_ADA = LedgerAda.lovelaceValueOf newDeposit_ADA
            ------------------
                !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> value_Deposit_FT <> value_Deposit_ADA
            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
     ----------------
        isCorrect_Output_SellOffer_Value_With_Withdraw ::  Integer ->Integer ->  Bool
        isCorrect_Output_SellOffer_Value_With_Withdraw !newWithdraw_FT !newWithdraw_ADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
            ------------------
                !value_Withdraw_FT = LedgerValue.assetClassValue fundFT_TN_AC newWithdraw_FT
                !value_Withdraw_ADA = LedgerAda.lovelaceValueOf newWithdraw_ADA
            ------------------
                !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> negate value_Withdraw_FT <> negate value_Withdraw_ADA
            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
     ----------------
        isCorrect_Output_SellOffer_Value_With_SwapFTxADA ::  Integer -> Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Value_With_SwapFTxADA !amount_FT !amount_ADA !commission_ADA =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
            ------------------
                !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC amount_FT
                !value_Amount_ADA = LedgerAda.lovelaceValueOf (amount_ADA - commission_ADA)
            ------------------
                !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> value_Amount_FT <> negate value_Amount_ADA
            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
     ----------------
        isCorrect_Output_SellOffer_Value_With_SwapADAxFT ::  Integer -> Integer -> Integer -> Bool
        isCorrect_Output_SellOffer_Value_With_SwapADAxFT !amount_ADA !amount_FT !commission_FT =
            let !output_Own_TxOut_And_SellOffer_Datum = getOutput_Own_TxOut_And_SellOffer_Datum
            ------------------
                !valueOf_SellOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SellOffer_Datum
            ------------------
                !value_Amount_ADA = LedgerAda.lovelaceValueOf amount_ADA
                !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC (amount_FT - commission_FT)
            ------------------
                !valueFor_SellOffer_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> value_Amount_ADA <> negate value_Amount_FT
            in  valueOf_SellOffer_Out `OnChainHelpers.isEqValue` valueFor_SellOffer_Out_Control
     ----------------
        isCorrect_Oracle_Signature :: T.Oracle_Data -> Ledger.Signature ->  Bool
        isCorrect_Oracle_Signature oracle_Data oracle_Signature= 
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
                !priceData = OnChainHelpers.oracleDataToBBS oracle_Data
            ------------------
            in  case OnChainHelpers.checkSignature oraclePaymentPubKey priceData oracle_Signature of
                    Left _ -> False
                    Right _ -> True
    ------------------
        isCorrect_Oracle_InRangeTime :: T.Oracle_Data -> Bool
        isCorrect_Oracle_InRangeTime oracle_Data  =  
            let
                -- TODO el tiempo de validez deberia venir en el datum del protocolo
                validRange = LedgerApiV2.txInfoValidRange info
                newLowerLimitValue :: LedgerApiV2.POSIXTime
                newLowerLimitValue = case Ledger.ivFrom validRange of
                    Ledger.LowerBound (Ledger.Finite a) True -> a - T.oracleData_Valid_Time
                    _-> traceError "Interval has no lower bound"
                
                newInterval = Ledger.Interval (Ledger.LowerBound (Ledger.Finite newLowerLimitValue) True) (Ledger.ivTo validRange )
                -- TODO: la valides de la transaccion hay que ponerla en 3 minutos
            in
                T.odTime oracle_Data `Ledger.member` newInterval
     ------------------
        isCorrect_Conversion :: T.Oracle_Data -> Integer -> Integer -> Bool
        isCorrect_Conversion oracle_Data !amount_FT !amount_ADA =  
            let
               (cs, tn, priceADA) = head $ T.iuValues $ T.odFTPriceADA oracle_Data
               price_FT_in_ADA = 
                    if cs == fundPolicy_CS && tn == T.fundFT_TN 
                        then priceADA
                        else traceError "FT Price ADA not found in Oracle Data"
            in
                amount_ADA == amount_FT * price_FT_in_ADA
     ------------------
        isAmount_FT_Available:: Integer -> Bool
        isAmount_FT_Available !amount_FT =  
            let
                !amount_FT_Available = T.sodAmount_FT_Available sellOffer_Datum_In
            in
                amount_FT_Available >= amount_FT
    ------------------
        isAmount_ADA_Available:: Integer -> Bool
        isAmount_ADA_Available !amount_ADA =  
            let
                !amount_ADA_Available = T.sodAmount_ADA_Available sellOffer_Datum_In
            in
                amount_ADA_Available >= amount_ADA
    ------------------
        isCorrect_Commission :: Integer -> Integer -> Bool
        isCorrect_Commission !swap_Amount !commission_Payed  =  
            -- las comisiones son en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
            -- eso significa que al valor de Commission_Rate_InBPx1e3 tengo que dividirlo por 
            -- 10e2 para pasarlo a bp
            -- 100 para pasarlo a porcentaje normal del 1 al 100
            -- 100 para pasarlo a porcentaje del 0 al 1
            -- den = 1e3 * 100 * 100 = 1000 * 100 * 100 = 10_000_000
            let
                com = T.sodAskedCommission_Rate_InBPx1e3 sellOffer_Datum_In
                den = 10_000_000
            in
                (swap_Amount * com) `divide` den == commission_Payed
    ------------------
        isBurningSellOfferID :: Bool
        isBurningSellOfferID  =
            let !sellOfferID_AC = LedgerValue.AssetClass (sellOfferPolicyID_CS, T.sellOfferID_TN)
            in OnChainHelpers.isNFT_Burning_With_AC sellOfferID_AC info

        
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
