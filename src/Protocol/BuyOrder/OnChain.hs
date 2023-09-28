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
module Protocol.BuyOrder.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import           Data.Aeson                (Value (Bool))
import qualified Ledger
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Data.Maybe as DataMaybe

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants         as T
import qualified Generic.OnChainHelpers    as OnChainHelpers
import qualified Generic.Types             as T
import qualified Protocol.BuyOrder.Types   as T
import qualified Protocol.Constants        as T
import qualified Protocol.Fund.Types       as FundT
import qualified Protocol.Protocol.Types   as ProtocolT
import qualified Protocol.Types            as T
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified PlutusTx.AssocMap as TxAssocMap

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
        !buyOrder_Validator_Hash = ppBuyOrder_Validator_Hash
    ------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
     ------------------
        !buyOrderPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
    ------------------
        !valueFor_Mint_BuyOrder_ID = LedgerValue.assetClassValue buyOrder_ID_AC 1
    ---------------------
        !buyOrder_ID_AC = LedgerValue.AssetClass (buyOrderPolicyID_CS, T.buyOrderID_TN)
    ------------------
    in  if traceIfFalse "" useThisToMakeScriptUnique
            &&
            case redeemer of
                T.PolicyRedeemerCreateBuyOrderID _ ->
                    -- que se mintee ID de Buy Order, con esta poliza, 1 unidad, con nombre de token que venga en datum del protocolo
                    -- que se cree datum correcto (parametros dentro de rangos permitidos, totales en cero, seller que firma la tx)
                    -- que vaya a la direccion del contrato correcta. La direccion puede estar en el datum del protocolo
                    traceIfFalse "not isMintingBuyOrderID" isMintingBuyOrderID &&
                    traceIfFalse "not isCorrect_Output_Address" isCorrect_Output_Address &&
                    traceIfFalse "not isCorrect_Output_BuyOrder_Datum" isCorrect_Output_BuyOrder_Datum &&
                    traceIfFalse "not isCorrect_Output_BuyOrder_Value" isCorrect_Output_BuyOrder_Value
                    ---------------------
                    where
                    ------------------
                        !outputs_TxOuts_And_BuyOrder_Datums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_Outputs_By_AC @T.ValidatorDatum @T.BuyOrder_DatumType ctx buyOrder_ID_AC T.getBuyOrder_DatumType
                        !output_TxOut_And_BuyOrder_Datum = case outputs_TxOuts_And_BuyOrder_Datums of
                            [x] -> x
                            _   -> traceError "expected exactly one BuyOrder output"
                    ---------------------
                        !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_BuyOrder_Datum
                    ---------------------
                        isMintingBuyOrderID :: Bool
                        isMintingBuyOrderID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_BuyOrder_ID
                    -----------------
                        isCorrect_Output_Address :: Bool
                        isCorrect_Output_Address =
                            let
                            ------------------
                                !buyOrder_Datum_Address = OnChainHelpers.getAddress_In_TxOut_And_Datum output_TxOut_And_BuyOrder_Datum
                            ------------------
                                !buyOrder_Datum_Address_Hash = OnChainHelpers.getUnsafeScriptHash_In_Address buyOrder_Datum_Address
                            ------------------
                            in buyOrder_Datum_Address_Hash == buyOrder_Validator_Hash
                    ------------------
                        isCorrect_Output_BuyOrder_Datum :: Bool
                        isCorrect_Output_BuyOrder_Datum =
                             -- TODO controlar que bodOfferedCommission_Rate_InBPx1e3 este dentro del rango permitido en el protocolo
                            let !buyOrder_Datum_Out_Control =
                                    T.mkBuyOrder_DatumType
                                        buyOrderPolicyID_CS
                                        (T.bodFundPolicy_CS buyOrder_Datum_Out)
                                        (T.bodBuyerPaymentPKH buyOrder_Datum_Out)
                                        (T.bodBuyerStakePKH buyOrder_Datum_Out)
                                        (T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_Out)
                                        0
                                        0
                                        T.buyOrder_Status_Open
                                        (T.bodMinADA buyOrder_Datum_Out)
                            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
                    ------------------
                        isCorrect_Output_BuyOrder_Value :: Bool
                        isCorrect_Output_BuyOrder_Value =
                            let
                                !valueFor_BuyOrder_Datum' = valueFor_Mint_BuyOrder_ID
                                !minADA_For_BuyOrder_Datum = T.bodMinADA buyOrder_Datum_Out
                                -- !minADA_For_BuyOrder_Datum = OnChainHelpers.calculateMinADAOfValue valueFor_BuyOrder_Datum' True
                                !value_MinADA_For_BuyOrder_Datum = LedgerAda.lovelaceValueOf minADA_For_BuyOrder_Datum
                                !valueFor_BuyOrder_Datum_Out_Control = valueFor_BuyOrder_Datum' <> value_MinADA_For_BuyOrder_Datum
                            ---------------------
                                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_BuyOrder_Datum
                            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Datum_Out_Control
                    ------------------
                T.PolicyRedeemerDeleteBuyOrderID _ ->
                    -- que se queme ID del Buy Order, 1 unidad. Creo que con esto es suficiente.
                    -- que se este ejecutando validador correcto. No seria necesario. Si se quema es por que sale de algun lado.
                    ---------------------
                        traceIfFalse "not isBurningBuyOrderID" isBurningBuyOrderID
                    ---------------------
                    where
                    ------------------
                        !valueFor_Burn_BuyOrder_ID = LedgerValue.assetClassValue buyOrder_ID_AC (negate 1)
                    ---------------------
                        isBurningBuyOrderID :: Bool
                        isBurningBuyOrderID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_BuyOrder_ID
                    -----------------
            then ()
            else error ()


--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_StatusChanged #-}
mkUpdated_BuyOrder_Datum_With_StatusChanged :: T.BuyOrder_DatumType -> Integer -> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_StatusChanged !buyOrder_Datum_In !newStatus =
    T.mkBuyOrder_DatumType
       (T.bodBuyOrderPolicyID_CS buyOrder_Datum_In)
       (T.bodFundPolicy_CS buyOrder_Datum_In)
        (T.bodBuyerPaymentPKH buyOrder_Datum_In)
        (T.bodBuyerStakePKH buyOrder_Datum_In)
        (T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_In)
        (T.bodFT_Received buyOrder_Datum_In)
        (T.bodFT_PayedAsCommission buyOrder_Datum_In)
        newStatus
        (T.bodMinADA buyOrder_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_CommissionChanged #-}
mkUpdated_BuyOrder_Datum_With_CommissionChanged :: T.BuyOrder_DatumType -> Integer -> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_CommissionChanged !buyOrder_Datum_In !newCommissionRate =
    T.mkBuyOrder_DatumType
       (T.bodBuyOrderPolicyID_CS buyOrder_Datum_In)
       (T.bodFundPolicy_CS buyOrder_Datum_In)
        (T.bodBuyerPaymentPKH buyOrder_Datum_In)
        (T.bodBuyerStakePKH buyOrder_Datum_In)
        newCommissionRate
        (T.bodFT_Received buyOrder_Datum_In)
        (T.bodFT_PayedAsCommission buyOrder_Datum_In)
        (T.bodOrder_Status buyOrder_Datum_In)
        (T.bodMinADA buyOrder_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_MinADAChanged #-}
mkUpdated_BuyOrder_Datum_With_MinADAChanged :: T.BuyOrder_DatumType -> Integer -> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_MinADAChanged !buyOrder_Datum_In !newMinADA =
    T.mkBuyOrder_DatumType
       (T.bodBuyOrderPolicyID_CS buyOrder_Datum_In)
       (T.bodFundPolicy_CS buyOrder_Datum_In)
        (T.bodBuyerPaymentPKH buyOrder_Datum_In)
        (T.bodBuyerStakePKH buyOrder_Datum_In)
        (T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_In)
        (T.bodFT_Received buyOrder_Datum_In)
        (T.bodFT_PayedAsCommission buyOrder_Datum_In)
        (T.bodOrder_Status buyOrder_Datum_In)
        newMinADA

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_Deposit #-}
mkUpdated_BuyOrder_Datum_With_Deposit :: T.BuyOrder_DatumType -> T.InvestUnit ->  T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_Deposit !buyOrder_Datum_In !newDeposit  =
    -- TODO: creo que no hay cambios en el datum. No hace falta este metodo
    T.mkBuyOrder_DatumType
       (T.bodBuyOrderPolicyID_CS buyOrder_Datum_In)
       (T.bodFundPolicy_CS buyOrder_Datum_In)
        (T.bodBuyerPaymentPKH buyOrder_Datum_In)
        (T.bodBuyerStakePKH buyOrder_Datum_In)
        (T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_In)
        (T.bodFT_Received buyOrder_Datum_In)
        (T.bodFT_PayedAsCommission buyOrder_Datum_In)
        (T.bodOrder_Status buyOrder_Datum_In)
        (T.bodMinADA buyOrder_Datum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_Withdraw #-}
mkUpdated_BuyOrder_Datum_With_Withdraw :: T.BuyOrder_DatumType -> T.InvestUnit-> T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_Withdraw !buyOrder_Datum_In !newWithdraw  =
    -- TODO: creo que no hay cambios en el datum. No hace falta este metodo
    T.mkBuyOrder_DatumType
       (T.bodBuyOrderPolicyID_CS buyOrder_Datum_In)
       (T.bodFundPolicy_CS buyOrder_Datum_In)
        (T.bodBuyerPaymentPKH buyOrder_Datum_In)
        (T.bodBuyerStakePKH buyOrder_Datum_In)
        (T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_In)
        (T.bodFT_Received buyOrder_Datum_In )
        (T.bodFT_PayedAsCommission buyOrder_Datum_In )
        (T.bodOrder_Status buyOrder_Datum_In)
        (T.bodMinADA buyOrder_Datum_In)

     --------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_BuyOrder_Datum_With_FillOrder #-}
mkUpdated_BuyOrder_Datum_With_FillOrder :: T.BuyOrder_DatumType -> Integer ->T.InvestUnit->   Integer ->T.BuyOrder_DatumType
mkUpdated_BuyOrder_Datum_With_FillOrder !buyOrder_Datum_In  !amount_FT !amount_Tokens !commission_FT=
    T.mkBuyOrder_DatumType
       (T.bodBuyOrderPolicyID_CS buyOrder_Datum_In)
       (T.bodFundPolicy_CS buyOrder_Datum_In)
        (T.bodBuyerPaymentPKH buyOrder_Datum_In)
        (T.bodBuyerStakePKH buyOrder_Datum_In)
        (T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_In)
        (T.bodFT_Received buyOrder_Datum_In + (amount_FT - commission_FT))
        (T.bodFT_PayedAsCommission buyOrder_Datum_In + commission_FT)
        (T.bodOrder_Status buyOrder_Datum_In)
        (T.bodMinADA buyOrder_Datum_In)

--------------------------------------------------------------------------------2
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
        -- !buyOrderPolicyID_CS = vpBuyOrderPolicyID_CS
     ------------------
        !input_Current_TxOut = OnChainHelpers.getUnsafe_Current_Input_TxOut ctx
    ------------------
        !buyOrder_Datum_In = T.getBuyOrder_DatumType datum
    ------------------
        !buyOrderPolicyID_CS = T.bodBuyOrderPolicyID_CS buyOrder_Datum_In
     ------------------
        !fundPolicy_CS = T.bodFundPolicy_CS buyOrder_Datum_In
        !fundFT_TN_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundFT_TN)
    ---------------------
        -- TODO: para hacer la politica unica con respecto al protocolo
        !useThisToMakeScriptUnique = True -- No hace falta uso las dos variables en la poliza
    ------------------
        redeemerUpdateOrderStatus = 1
        redeemerUpdateOfferedCommissionRate = 2
        redeemerUpdateMinADA = 3
        redeemerDeposit = 4
        redeemerWithdraw = 5
        redeemerFillOrder = 6
        redeemerDelete = 7
    ------------------
        getRedeemerType :: Integer
        getRedeemerType = case redeemer of
            (T.ValidatorRedeemerUpdateOrderStatus _)           -> redeemerUpdateOrderStatus
            (T.ValidatorRedeemerUpdateOfferedCommissionRate _) -> redeemerUpdateOfferedCommissionRate
            (T.ValidatorRedeemerUpdateMinADA _)                -> redeemerUpdateMinADA
            (T.ValidatorRedeemerDeposit _)                     -> redeemerDeposit
            (T.ValidatorRedeemerWithdraw _)                    -> redeemerWithdraw
            (T.ValidatorRedeemerFillOrder _)               -> redeemerFillOrder
            (T.ValidatorRedeemerDelete _)                      -> redeemerDelete
    ------------------
        validateRedeemer :: Integer -> Bool
        validateRedeemer redeemerType
            | redeemerType == redeemerUpdateOrderStatus = validateAdminAction (T.bodBuyerPaymentPKH buyOrder_Datum_In) && validateUpdateOrderStatus redeemer
            | redeemerType == redeemerUpdateOfferedCommissionRate  = validateAdminAction (T.bodBuyerPaymentPKH buyOrder_Datum_In) && validatUpdateOfferedCommissionRate redeemer
            | redeemerType == redeemerUpdateMinADA = validateAdminAction (T.bodBuyerPaymentPKH buyOrder_Datum_In) && validateUpdateMinADA redeemer
            | redeemerType == redeemerDeposit = validateAdminAction (T.bodBuyerPaymentPKH buyOrder_Datum_In) && validateDeposit redeemer
            | redeemerType == redeemerWithdraw = validateAdminAction (T.bodBuyerPaymentPKH buyOrder_Datum_In) && validateWithdraw redeemer
            | redeemerType == redeemerFillOrder = validateFillOrder redeemer
            | redeemerType == redeemerDelete = validateAdminAction (T.bodBuyerPaymentPKH buyOrder_Datum_In) && validateDelete
            | otherwise = False
    ------------------
        validateAdminAction :: T.WalletPaymentPKH -> Bool
        validateAdminAction !admin =
                traceIfFalse "not isSignedByAny admin" (LedgerContextsV2.txSignedBy info admin)
                && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTimeRange)
    ------------------
        validateUpdateOrderStatus ::  T.ValidatorRedeemer  -> Bool
        validateUpdateOrderStatus (T.ValidatorRedeemerUpdateOrderStatus T.ValidatorRedeemerUpdateOrderStatusType{..})  =
        ---- change status to open or close. Only buyer can do it
        -- check that there is one input and one output in this contract
        -- check datum update with new status
        -- check value not changed
            traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_StatusChanged" (isCorrect_Output_BuyOrder_Datum_With_StatusChanged ruosNewStatus)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Value_NotChanged" isCorrect_Output_BuyOrder_Value_NotChanged
        validateUpdateOrderStatus _   = False
    ------------------
        validatUpdateOfferedCommissionRate ::  T.ValidatorRedeemer  -> Bool
        validatUpdateOfferedCommissionRate (T.ValidatorRedeemerUpdateOfferedCommissionRate T.ValidatorRedeemerUpdateOfferedCommissionRateType{..})  =
        ---- change offered commission rate. Only buyer can do it. Must be in the range of protocol datum commissionRateForBuyOrdersRange
        -- check that there is one input and one output in this contract
        -- check datum update with new commissions rate. Must be in the range of protocol datum commissionRateForBuyOrdersRange
        -- check value not changed
            traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_CommissionChanged" (isCorrect_Output_BuyOrder_Datum_With_CommissionChanged rucrNewCommissionRate)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Value_NotChanged" isCorrect_Output_BuyOrder_Value_NotChanged
        validatUpdateOfferedCommissionRate _   = False
    ------------------
        validateUpdateMinADA ::  T.ValidatorRedeemer  -> Bool
        validateUpdateMinADA (T.ValidatorRedeemerUpdateMinADA T.ValidatorRedeemerUpdateMinADAType{..})  =
        ---- change min ada value. Only buyer can do it
        -- check that there is one input and one output in this contract
        -- check datum update with new minAda
        -- check value changed ADA
            traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_MinADAChanged" (isCorrect_Output_BuyOrder_Datum_With_MinADAChanged rumaNewMinADA)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_MinADAChanged" (isCorrect_Output_BuyOrder_Value_With_MinADAChanged rumaNewMinADA)
        validateUpdateMinADA _   = False
    ------------------
        validateDeposit :: T.ValidatorRedeemer  -> Bool
        validateDeposit  (T.ValidatorRedeemerDeposit T.ValidatorRedeemerDepositType{..})  =
        ----  add Tokens. Only buyer can do it.
        -- check that there is one input and one output in this contract
        -- check datum update with withdraw ... me parece que no hay cambios que se hagan en el datum en esta tx
        -- check value changed with withdraw
            traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_Deposit" (isCorrect_Output_BuyOrder_Datum_With_Deposit rdNewDeposit)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_Deposit" (isCorrect_Output_BuyOrder_Value_With_Deposit rdNewDeposit)
        validateDeposit _   = False
    ------------------
        validateWithdraw ::  T.ValidatorRedeemer  -> Bool
        validateWithdraw (T.ValidatorRedeemerWithdraw T.ValidatorRedeemerWithdrawType{..})  =
        ---- get back some FT or Tokens. Only buyer can do it.
        -- check that there is one input and one output in this contract
        -- check datum update with withdraw ... me parece que no hay cambios que se hagan en el datum en esta tx
        -- check value changed with withdraw
            traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_Withdraw" (isCorrect_Output_BuyOrder_Datum_With_Withdraw rwNewWithdraw)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_Withdraw" (isCorrect_Output_BuyOrder_Value_With_Withdraw rwNewWithdraw)
        validateWithdraw _   = False
    ------------------
        validateFillOrder ::  T.ValidatorRedeemer  -> Bool
        validateFillOrder (T.ValidatorRedeemerFillOrder T.ValidatorRedeemerFillOrderType{..})  =
        ---- if order is open, user give FT and get Tokens and commission. Use a price for conversion FT to ADA and tokens offered to ADA provided by oracle. Must check signatura and validity time
        -- check that there is one input and one output in this contract
        -- check datum update with swap. Totals calculated
        -- check commissions
        -- check value changed FT and ADA
        -- check price, validity time and signature
            traceIfFalse "not isOrderOpen" isOrderOpen
            && traceIfFalse "not isCorrect_Oracle_Signature" (isCorrect_Oracle_Signature rsaxfOracle_Data rsaxfOracle_Signature)
            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (isCorrect_Oracle_InRangeTime rsaxfOracle_Data )
            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsaxfOracle_Data rsAmount_Tokens rsAmount_FT )
            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsAmount_FT rsCommission_FT)
            && traceIfFalse "not isAmount_Tokens_Available" (isAmount_Tokens_Available rsAmount_Tokens)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Datum_With_FillOrder" (isCorrect_Output_BuyOrder_Datum_With_FillOrder rsAmount_FT rsAmount_Tokens rsCommission_FT)
            && traceIfFalse "not isCorrect_Output_BuyOrder_Value_With_FillOrder" (isCorrect_Output_BuyOrder_Value_With_FillOrder rsAmount_FT rsAmount_Tokens rsCommission_FT)
        validateFillOrder _   = False
    ------------------
        validateDelete :: Bool
        validateDelete = traceIfFalse "not isBurningBuyOrderID" isBurningBuyOrderID
        ---- get back all FT and tokens and delete datum utxo. Burn order ID. only buyer can do it
        -- check that there is one input and zero output in this contract
        -- check that ID is burning
    ------------------
        isOrderOpen :: Bool
        isOrderOpen = T.bodOrder_Status buyOrder_Datum_In == T.buyOrder_Status_Open
     ------------------
        convertToMap :: T.InvestUnit -> TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer)
        convertToMap (T.InvestUnit tokens) = foldl insertToken TxAssocMap.empty tokens
            where
                insertToken acc (cs, tn, i) =
                    let innerMap = DataMaybe.fromMaybe TxAssocMap.empty (TxAssocMap.lookup cs acc)
                        updatedInnerMap = TxAssocMap.insert tn (maybe i (+ i) (TxAssocMap.lookup tn innerMap)) innerMap
                    in TxAssocMap.insert cs updatedInnerMap acc
        convertToValue :: T.InvestUnit -> LedgerValue.Value
        convertToValue iu = LedgerValue.Value (convertToMap iu)
    ------------------
        getOutput_Own_TxOut_And_BuyOrder_Datum :: (LedgerApiV2.TxOut, T.BuyOrder_DatumType)
        getOutput_Own_TxOut_And_BuyOrder_Datum =
            let
                !outputs_Own_TxOuts = OnChainHelpers.getUnsafe_Own_Outputs_TxOuts ctx
            ------------------
                !outputs_Own_TxOuts_And_BuyOrder_Datums = OnChainHelpers.getUnsafe_TxOuts_And_DatumTypes_from_TxOuts_By_CS @T.ValidatorDatum @T.BuyOrder_DatumType outputs_Own_TxOuts ctx buyOrderPolicyID_CS T.getBuyOrder_DatumType
                !output_Own_TxOut_And_BuyOrder_Datum = case outputs_Own_TxOuts_And_BuyOrder_Datums of
                    [x] -> x
                    _   -> traceError "expected exactly one BuyOrder output"
            ------------------
            in output_Own_TxOut_And_BuyOrder_Datum
    ------------------
        isCorrect_Output_BuyOrder_Datum_With_StatusChanged :: Integer -> Bool
        isCorrect_Output_BuyOrder_Datum_With_StatusChanged !newStatus =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_StatusChanged buyOrder_Datum_In newStatus
            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
    ------------------
        isCorrect_Output_BuyOrder_Datum_With_CommissionChanged :: Integer -> Bool
        isCorrect_Output_BuyOrder_Datum_With_CommissionChanged !newCommissionRate =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_CommissionChanged buyOrder_Datum_In newCommissionRate
            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
    ------------------
        isCorrect_Output_BuyOrder_Datum_With_MinADAChanged:: Integer -> Bool
        isCorrect_Output_BuyOrder_Datum_With_MinADAChanged !newMinADA =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_MinADAChanged buyOrder_Datum_In newMinADA
            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
    ------------------
        isCorrect_Output_BuyOrder_Datum_With_Deposit:: T.InvestUnit -> Bool
        isCorrect_Output_BuyOrder_Datum_With_Deposit !newDeposit  =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_Deposit buyOrder_Datum_In newDeposit
            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
    ------------------
        isCorrect_Output_BuyOrder_Datum_With_Withdraw:: T.InvestUnit -> Bool
        isCorrect_Output_BuyOrder_Datum_With_Withdraw !newWithdraw =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_Withdraw buyOrder_Datum_In newWithdraw
            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
    ------------------
        isCorrect_Output_BuyOrder_Datum_With_FillOrder:: Integer -> T.InvestUnit -> Integer -> Bool
        isCorrect_Output_BuyOrder_Datum_With_FillOrder !amount_FT !amount_Tokens !commission_FT =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !buyOrder_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !buyOrder_Datum_Out_Control = mkUpdated_BuyOrder_Datum_With_FillOrder buyOrder_Datum_In amount_FT amount_Tokens commission_FT
            in  buyOrder_Datum_Out `OnChainHelpers.isUnsafeEqDatums` buyOrder_Datum_Out_Control
    ------------------
        isCorrect_Output_BuyOrder_Value_NotChanged :: Bool
        isCorrect_Output_BuyOrder_Value_NotChanged =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut
            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
     ----------------
        isCorrect_Output_BuyOrder_Value_With_MinADAChanged ::  Integer -> Bool
        isCorrect_Output_BuyOrder_Value_With_MinADAChanged !newMinADA =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !value_MinADA_BuyOrder_Datum_In = LedgerAda.lovelaceValueOf (T.bodMinADA buyOrder_Datum_In)
                !value_MinADA_BuyOrder_Datum_Out = LedgerAda.lovelaceValueOf newMinADA
            ------------------
                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> negate value_MinADA_BuyOrder_Datum_In <> value_MinADA_BuyOrder_Datum_Out
            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
    ----------------
        isCorrect_Output_BuyOrder_Value_With_Deposit ::   T.InvestUnit ->  Bool
        isCorrect_Output_BuyOrder_Value_With_Deposit !newDeposit =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !value_Deposit_Tokens = convertToValue newDeposit
            ------------------
                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> value_Deposit_Tokens 
            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
     ----------------
        isCorrect_Output_BuyOrder_Value_With_Withdraw ::   T.InvestUnit ->  Bool
        isCorrect_Output_BuyOrder_Value_With_Withdraw !newWithdraw =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !value_Withdraw_Tokens = convertToValue newWithdraw
            ------------------
                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> negate value_Withdraw_Tokens 
            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
     ----------------
        isCorrect_Output_BuyOrder_Value_With_FillOrder ::  Integer ->  T.InvestUnit ->  Integer -> Bool
        isCorrect_Output_BuyOrder_Value_With_FillOrder !amount_FT  !amount_Tokens !commission_FT  =
            let !output_Own_TxOut_And_BuyOrder_Datum = getOutput_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !valueOf_BuyOrder_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_BuyOrder_Datum
            ------------------
                !value_FillOrder_Tokens = convertToValue amount_Tokens
                !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC (amount_FT - commission_FT)
            ------------------
                !valueFor_BuyOrder_Out_Control = LedgerApiV2.txOutValue input_Current_TxOut <> value_Amount_FT <> negate value_FillOrder_Tokens
            in  valueOf_BuyOrder_Out `OnChainHelpers.isEqValue` valueFor_BuyOrder_Out_Control
     ----------------
        isCorrect_Oracle_Signature :: T.Oracle_Data -> Ledger.Signature ->  Bool
        isCorrect_Oracle_Signature oracle_Data oracle_Signature =
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
        -- Calculates the total price of swapTokens based on oraclePrices
        totalSwapPrice :: TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -- ^ Oracle prices
                    -> TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -- ^ Swap tokens
                    -> Integer -- ^ Total price
        totalSwapPrice oraclePrices swapTokens = 
            PlutusTx.Prelude.foldl (\acc cs ->
                PlutusTx.Prelude.foldl (\innerAcc (tn, amount) ->
                    let price = DataMaybe.fromMaybe 0 (TxAssocMap.lookup cs oraclePrices >>= TxAssocMap.lookup tn)
                    in innerAcc + (price * amount)
                ) 0 (TxAssocMap.toList $ DataMaybe.fromMaybe TxAssocMap.empty (TxAssocMap.lookup cs swapTokens)) + acc
            ) 0 (TxAssocMap.keys swapTokens)
     ------------------
        -- Looks up the price of a specific token in the oracle prices
        lookupPrice :: LedgerApiV2.CurrencySymbol 
                    -> LedgerApiV2.TokenName 
                    -> TxAssocMap.Map LedgerApiV2.CurrencySymbol (TxAssocMap.Map LedgerApiV2.TokenName Integer) -- ^ Oracle prices
                    -> Maybe Integer
        lookupPrice cs tn oraclePrices =
            TxAssocMap.lookup cs oraclePrices >>= TxAssocMap.lookup tn
     ------------------
        isCorrect_Conversion :: T.Oracle_Data -> T.InvestUnit -> Integer -> Bool
        isCorrect_Conversion oracle_Data !amount_Tokens !amount_FT =
            let
                oraclePrices = convertToMap $ T.odFTPriceADA oracle_Data
                swapTokens = convertToMap amount_Tokens
                price_Tokens_in_ADA = totalSwapPrice oraclePrices swapTokens
                price_FT_in_ADA = case lookupPrice fundPolicy_CS T.fundFT_TN oraclePrices of
                        Just priceADA -> priceADA
                        _ -> traceError "FT Price ADA not found in Oracle Data"
            in
                price_Tokens_in_ADA == amount_FT * price_FT_in_ADA
     ------------------
        isCorrect_Commission :: Integer -> Integer -> Bool
        isCorrect_Commission !amount_FT !commission_FT_Payed  =
            -- las comisiones son en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
            -- eso significa que al valor de Commission_Rate_InBPx1e3 tengo que dividirlo por 
            -- 10e2 para pasarlo a bp
            -- 100 para pasarlo a porcentaje normal del 1 al 100
            -- 100 para pasarlo a porcentaje del 0 al 1
            -- den = 1e3 * 100 * 100 = 1000 * 100 * 100 = 10_000_000
            let
                com = T.bodOfferedCommission_Rate_InBPx1e3 buyOrder_Datum_In
                den = 10_000_000
            in
                (amount_FT * com) `divide` den  == commission_FT_Payed
    ------------------
        isAmount_Tokens_Available :: T.InvestUnit -> Bool
        isAmount_Tokens_Available !amount_Tokens =
            let
                !value_Swap_Tokens = convertToValue amount_Tokens
            in
                value_Swap_Tokens `OnChainHelpers.isIncludeValue` LedgerApiV2.txOutValue input_Current_TxOut 
    ------------------
        isBurningBuyOrderID :: Bool
        isBurningBuyOrderID  =
            let !buyOrderID_AC = LedgerValue.AssetClass (buyOrderPolicyID_CS, T.buyOrderID_TN)
            in OnChainHelpers.isNFT_Burning_With_AC buyOrderID_AC info
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
