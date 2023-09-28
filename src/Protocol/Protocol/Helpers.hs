{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}

--------------------------------------------------------------------------------2
module Protocol.Protocol.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import           PlutusTx.Prelude
--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types           as T
import qualified Protocol.Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_ProtocolDatum_With_NormalChanges #-}
mkUpdated_ProtocolDatum_With_NormalChanges :: T.ProtocolDatumType -> LedgerAddress.PaymentPubKey -> [T.WalletPaymentPKH] -> [T.FundClass] -> T.MinMaxDef LedgerApiV2.POSIXTime -> Integer -> Integer -> T.MinMaxDef Integer -> T.MinMaxDef Integer -> T.MinMaxDef Integer -> Integer -> Integer -> Integer -> [T.WalletPaymentPKH] -> T.ProtocolDatumType
mkUpdated_ProtocolDatum_With_NormalChanges !protocolDatum_In !oraclePaymentPubKey !admins !fundClasses !fundLifeTime !requiredMAYZForSellOffers !requiredMAYZForBuyOrders !commissionFunds !commissionSellOffers !commissionBuyOrders !share_Protocol !share_MAYZ !share_FundAdmins !mayzWallets =
    T.mkProtocolDatumType
        (T.pdScriptPolicyID_CS protocolDatum_In)
        (T.pdScriptValidator_Hash protocolDatum_In)
        oraclePaymentPubKey
        admins
        fundClasses
        fundLifeTime
        requiredMAYZForSellOffers
        requiredMAYZForBuyOrders
        commissionFunds
        commissionSellOffers
        commissionBuyOrders
        share_Protocol
        share_MAYZ
        share_FundAdmins
        mayzWallets
        (T.pdMinADA protocolDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_ProtocolDatum_With_FundAdded #-}
mkUpdated_ProtocolDatum_With_FundAdded :: T.ProtocolDatumType -> T.ProtocolDatumType
mkUpdated_ProtocolDatum_With_FundAdded !protocolDatum_In  =
    T.mkProtocolDatumType
        (T.pdScriptPolicyID_CS protocolDatum_In)
        (T.pdScriptValidator_Hash protocolDatum_In)
        (T.pdOraclePaymentPubKey protocolDatum_In)
        (T.pdAdmins protocolDatum_In)
        (T.pdFundClasses protocolDatum_In)
        (T.pdFundLifeTime protocolDatum_In)
        (T.pdRequiredMAYZForSellOffers protocolDatum_In)
        (T.pdRequiredMAYZForBuyOrders protocolDatum_In)
        (T.pdCommissionFunds protocolDatum_In)
        (T.pdCommissionSellOffers protocolDatum_In)
        (T.pdCommissionBuyOrders protocolDatum_In)
        (T.pdShare_Protocol protocolDatum_In)
        (T.pdShare_MAYZ protocolDatum_In)
        (T.pdShare_FundAdmins protocolDatum_In)
        (T.pdMAYZWallets protocolDatum_In)
        (T.pdMinADA protocolDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_ProtocolDatum_With_FundDeleted #-}
mkUpdated_ProtocolDatum_With_FundDeleted :: T.ProtocolDatumType -> T.ProtocolDatumType
mkUpdated_ProtocolDatum_With_FundDeleted !protocolDatum_In  =
    T.mkProtocolDatumType
        (T.pdScriptPolicyID_CS protocolDatum_In)
        (T.pdScriptValidator_Hash protocolDatum_In)
        (T.pdOraclePaymentPubKey protocolDatum_In)
        (T.pdAdmins protocolDatum_In)
        (T.pdFundClasses protocolDatum_In)
        (T.pdFundLifeTime protocolDatum_In)
        (T.pdRequiredMAYZForSellOffers protocolDatum_In)
        (T.pdRequiredMAYZForBuyOrders protocolDatum_In)
        (T.pdCommissionFunds protocolDatum_In)
        (T.pdCommissionSellOffers protocolDatum_In)
        (T.pdCommissionBuyOrders protocolDatum_In)
        (T.pdShare_Protocol protocolDatum_In)
        (T.pdShare_MAYZ protocolDatum_In)
        (T.pdShare_FundAdmins protocolDatum_In)
        (T.pdMAYZWallets protocolDatum_In)
        (T.pdMinADA protocolDatum_In)

--------------------------------------------------------------------------------2
