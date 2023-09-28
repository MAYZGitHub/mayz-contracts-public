{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.BuyOrder.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Data.Aeson           as DataAeson
import qualified Data.OpenApi.Schema  as DataOpenApiSchema
import qualified GHC.Generics         as GHCGenerics
import qualified Ledger.Value         as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types        as T
import qualified Ledger
import           Protocol.Types       (InvestUnit)
import qualified Protocol.Types       as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------22

data PolicyParams
    = PolicyParams
          { ppProtocolPolicyID_CS     :: T.CS
          , ppBuyOrder_Validator_Hash :: LedgerApiV2.ValidatorHash
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 = ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2 &&
        ppBuyOrder_Validator_Hash p1 == ppBuyOrder_Validator_Hash p2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

newtype ValidatorParams
    = ValidatorParams { vpProtocolPolicyID_CS :: T.CS }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        vpProtocolPolicyID_CS p1 == vpProtocolPolicyID_CS p2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------22

data BuyOrder_DatumType
    = BuyOrder_DatumType
          { bodBuyOrderPolicyID_CS             :: T.CS
          , bodFundPolicy_CS                   :: T.CS
          , bodBuyerPaymentPKH                 :: T.WalletPaymentPKH
          , bodBuyerStakePKH                   :: Maybe T.WalletPaymentPKH
          , bodOfferedCommission_Rate_InBPx1e3 :: Integer
          , bodFT_Received                     :: Integer
          , bodFT_PayedAsCommission            :: Integer
          , bodOrder_Status                    :: Integer
          , bodMinADA                          :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq BuyOrder_DatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
            bodBuyOrderPolicyID_CS sd1 == bodBuyOrderPolicyID_CS sd2
            && bodFundPolicy_CS sd1 == bodFundPolicy_CS sd2
            && bodBuyerPaymentPKH sd1 == bodBuyerPaymentPKH sd2
            && bodBuyerStakePKH sd1 == bodBuyerStakePKH sd2
            && bodOfferedCommission_Rate_InBPx1e3 sd1 == bodOfferedCommission_Rate_InBPx1e3 sd2
            && bodFT_Received sd1 == bodFT_Received sd2
            && bodFT_PayedAsCommission sd1 == bodFT_PayedAsCommission sd2
                && bodOrder_Status sd1 == bodOrder_Status sd2
             && bodMinADA sd1 == bodMinADA sd2

PlutusTx.makeIsDataIndexed ''BuyOrder_DatumType [('BuyOrder_DatumType, 0)]

newtype ValidatorDatum
    = BuyOrder_Datum BuyOrder_DatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    BuyOrder_Datum sd1 == BuyOrder_Datum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('BuyOrder_Datum, 0)]

{-# INLINEABLE getBuyOrder_DatumType #-}
getBuyOrder_DatumType :: ValidatorDatum -> BuyOrder_DatumType
getBuyOrder_DatumType (BuyOrder_Datum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkBuyOrder_Datum #-}
mkBuyOrder_Datum :: T.CS -> T.CS ->T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer ->   Integer ->Integer -> Integer -> Integer -> ValidatorDatum
mkBuyOrder_Datum
    buyOrderPolicyID_CS
    fundPolicy_CS
    sellerPaymentPKH
    sellerStakePKH
    offeredCommission_Rate_InBPx1e3
    total_FT_Received
    total_FT_PayedAsCommission
    order_Status
    minADA
    =
        BuyOrder_Datum $
            mkBuyOrder_DatumType
                buyOrderPolicyID_CS
                fundPolicy_CS
                sellerPaymentPKH
                sellerStakePKH
                offeredCommission_Rate_InBPx1e3
                total_FT_Received
                total_FT_PayedAsCommission
                order_Status
                minADA

{-# INLINEABLE mkBuyOrder_DatumType #-}
mkBuyOrder_DatumType :: T.CS ->T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer ->  Integer -> Integer -> Integer -> Integer  -> BuyOrder_DatumType
mkBuyOrder_DatumType
    buyOrderPolicyID_CS
    fundPolicy_CS
    sellerPaymentPKH
    sellerStakePKH
    offeredCommission_Rate_InBPx1e3
    total_FT_Received
    total_FT_PayedAsCommission
    order_Status
    minADA
        =
        BuyOrder_DatumType
            {
                bodBuyOrderPolicyID_CS = buyOrderPolicyID_CS,
                bodFundPolicy_CS = fundPolicy_CS,
                bodBuyerPaymentPKH = sellerPaymentPKH,
                bodBuyerStakePKH = sellerStakePKH,
                bodOfferedCommission_Rate_InBPx1e3 = offeredCommission_Rate_InBPx1e3,
                bodFT_Received = total_FT_Received,
                bodFT_PayedAsCommission = total_FT_PayedAsCommission,
                bodOrder_Status = order_Status,
                bodMinADA = minADA
            }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PolicyRedeemerCreateBuyOrderType = PolicyRedeemerCreateBuyOrderType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerCreateBuyOrderType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerCreateBuyOrderType

data PolicyRedeemerDeleteBuyOrderType = PolicyRedeemerDeleteBuyOrderType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerDeleteBuyOrderType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerDeleteBuyOrderType

data PolicyRedeemer
    = PolicyRedeemerCreateBuyOrderID PolicyRedeemerCreateBuyOrderType
    | PolicyRedeemerDeleteBuyOrderID PolicyRedeemerDeleteBuyOrderType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerCreateBuyOrderID rmtx1 == PolicyRedeemerCreateBuyOrderID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerDeleteBuyOrderID rmtx1 == PolicyRedeemerDeleteBuyOrderID rmtx2 = rmtx1 == rmtx2
    _ == _                                                                       = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerCreateBuyOrderID, 1),
      ('PolicyRedeemerDeleteBuyOrderID, 2)
    ]

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

newtype ValidatorRedeemerUpdateOrderStatusType
    = ValidatorRedeemerUpdateOrderStatusType { ruosNewStatus :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateOrderStatusType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = ruosNewStatus r1 == ruosNewStatus r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateOrderStatusType [('ValidatorRedeemerUpdateOrderStatusType, 0)]

newtype ValidatorRedeemerUpdateOfferedCommissionRateType
    = ValidatorRedeemerUpdateOfferedCommissionRateType { rucrNewCommissionRate :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateOfferedCommissionRateType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rucrNewCommissionRate r1 == rucrNewCommissionRate r2


PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateOfferedCommissionRateType [('ValidatorRedeemerUpdateOfferedCommissionRateType, 0)]

newtype ValidatorRedeemerUpdateMinADAType
    = ValidatorRedeemerUpdateMinADAType { rumaNewMinADA :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rumaNewMinADA r1 == rumaNewMinADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

newtype ValidatorRedeemerDepositType
    = ValidatorRedeemerDepositType { rdNewDeposit :: InvestUnit }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rdNewDeposit r1 == rdNewDeposit r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

newtype ValidatorRedeemerWithdrawType
    = ValidatorRedeemerWithdrawType { rwNewWithdraw :: InvestUnit }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rwNewWithdraw r1 == rwNewWithdraw r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]

data ValidatorRedeemerFillOrderType
    = ValidatorRedeemerFillOrderType
          { rsAmount_Tokens       :: T.InvestUnit
          , rsAmount_FT           :: Integer
          , rsCommission_FT       :: Integer
          , rsaxfOracle_Data      :: T.Oracle_Data
          , rsaxfOracle_Signature :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFillOrderType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rsAmount_Tokens  r1 == rsAmount_Tokens r2 &&
        rsAmount_FT  r1 == rsAmount_FT r2 &&
        rsCommission_FT  r1 == rsCommission_FT r2 &&
        rsaxfOracle_Data  r1 == rsaxfOracle_Data r2 &&
        rsaxfOracle_Signature  r1 == rsaxfOracle_Signature r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerFillOrderType [('ValidatorRedeemerFillOrderType, 0)]

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

data ValidatorRedeemer
    = ValidatorRedeemerUpdateOrderStatus ValidatorRedeemerUpdateOrderStatusType
    | ValidatorRedeemerUpdateOfferedCommissionRate ValidatorRedeemerUpdateOfferedCommissionRateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerFillOrder ValidatorRedeemerFillOrderType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateOrderStatus rmf1 == ValidatorRedeemerUpdateOrderStatus rmf2                       = rmf1 == rmf2
    ValidatorRedeemerUpdateOfferedCommissionRate rmcp1 == ValidatorRedeemerUpdateOfferedCommissionRate rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2                               = rmcp1 == rmcp2
    ValidatorRedeemerDeposit rmcp1 == ValidatorRedeemerDeposit rmcp2                                         = rmcp1 == rmcp2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2                                       = rmcp1 == rmcp2
    ValidatorRedeemerFillOrder rmcp1 == ValidatorRedeemerFillOrder rmcp2                                     = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2                                           = rmcp1 == rmcp2
    _ == _                                                                                                   = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateOrderStatus, 0),
      ('ValidatorRedeemerUpdateOfferedCommissionRate, 1),
      ('ValidatorRedeemerUpdateMinADA, 2),
      ('ValidatorRedeemerDeposit, 3),
      ('ValidatorRedeemerWithdraw, 4),
      ('ValidatorRedeemerFillOrder, 5),
      ('ValidatorRedeemerDelete, 6)
    ]


------------------------------------------------------------------------------
