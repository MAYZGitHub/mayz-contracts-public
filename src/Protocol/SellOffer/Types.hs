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
module Protocol.SellOffer.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Data.Aeson           as DataAeson
import qualified Data.OpenApi.Schema  as DataOpenApiSchema
import qualified GHC.Generics         as GHCGenerics
import qualified Ledger
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
          { ppProtocolPolicyID_CS      :: T.CS
          , ppSellOffer_Validator_Hash :: LedgerApiV2.ValidatorHash
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 = ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2 &&
        ppSellOffer_Validator_Hash p1 == ppSellOffer_Validator_Hash p2

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

data SellOffer_DatumType
    = SellOffer_DatumType
          { sodSellOfferPolicyID_CS          :: T.CS
          , sodFundPolicy_CS                 :: T.CS
          , sodSellerPaymentPKH              :: T.WalletPaymentPKH
          , sodSellerStakePKH                :: Maybe T.WalletPaymentPKH
          , sodAskedCommission_Rate_InBPx1e3 :: Integer
          , sodAmount_FT_Available           :: Integer
          , sodAmount_ADA_Available          :: Integer
          , sodTotal_FT_Earned               :: Integer
          , sodTotal_ADA_Earned              :: Integer
          , sodOrder_Status                  :: Integer
          , sodMinADA                        :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq SellOffer_DatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
            sodSellOfferPolicyID_CS sd1 == sodSellOfferPolicyID_CS sd2
            && sodFundPolicy_CS sd1 == sodFundPolicy_CS sd2
            && sodSellerPaymentPKH sd1 == sodSellerPaymentPKH sd2
            && sodSellerStakePKH sd1 == sodSellerStakePKH sd2
            && sodAskedCommission_Rate_InBPx1e3 sd1 == sodAskedCommission_Rate_InBPx1e3 sd2
            && sodAmount_FT_Available sd1 == sodAmount_FT_Available sd2
            && sodAmount_ADA_Available sd1 == sodAmount_ADA_Available sd2
            && sodTotal_FT_Earned sd1 == sodTotal_FT_Earned sd2
            && sodTotal_ADA_Earned sd1 == sodTotal_ADA_Earned sd2
            && sodOrder_Status sd1 == sodOrder_Status sd2
            && sodMinADA sd1 == sodMinADA sd2

PlutusTx.makeIsDataIndexed ''SellOffer_DatumType [('SellOffer_DatumType, 0)]

newtype ValidatorDatum
    = SellOffer_Datum SellOffer_DatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    SellOffer_Datum sd1 == SellOffer_Datum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('SellOffer_Datum, 0)]

{-# INLINEABLE getSellOffer_DatumType #-}
getSellOffer_DatumType :: ValidatorDatum -> SellOffer_DatumType
getSellOffer_DatumType (SellOffer_Datum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkSellOffer_Datum #-}
mkSellOffer_Datum :: T.CS ->  T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer ->    Integer ->Integer ->    Integer ->Integer -> Integer -> Integer -> ValidatorDatum
mkSellOffer_Datum
    sellOfferPolicyID_CS
    fundPolicy_CS
    sellerPaymentPKH
    sellerStakePKH
    askedCommission_Rate_InBPx1e3
    amount_FT_Available
    amount_ADA_Available
    total_FT_Earned
    total_ADA_Earned
    order_Status
    minADA
    =
        SellOffer_Datum $
            mkSellOffer_DatumType
                sellOfferPolicyID_CS
                fundPolicy_CS
                sellerPaymentPKH
                sellerStakePKH
                askedCommission_Rate_InBPx1e3
                amount_FT_Available
                amount_ADA_Available
                total_FT_Earned
                total_ADA_Earned
                order_Status
                minADA

{-# INLINEABLE mkSellOffer_DatumType #-}
mkSellOffer_DatumType ::  T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer -> Integer ->    Integer ->Integer -> Integer -> Integer -> Integer  -> SellOffer_DatumType
mkSellOffer_DatumType
    sellOfferPolicyID_CS
    fundPolicy_CS
    sellerPaymentPKH
    sellerStakePKH
    askedCommission_Rate_InBPx1e3
    amount_FT_Available
    amount_ADA_Available
    total_FT_Earned
    total_ADA_Earned
    order_Status
    minADA
        =
        SellOffer_DatumType
            {
                sodSellOfferPolicyID_CS = sellOfferPolicyID_CS,
                sodFundPolicy_CS = fundPolicy_CS,
                sodSellerPaymentPKH = sellerPaymentPKH,
                sodSellerStakePKH = sellerStakePKH,
                sodAskedCommission_Rate_InBPx1e3 = askedCommission_Rate_InBPx1e3,
                sodAmount_FT_Available =  amount_FT_Available,
                sodAmount_ADA_Available =    amount_ADA_Available,
                sodTotal_FT_Earned = total_FT_Earned,
                sodTotal_ADA_Earned = total_ADA_Earned,
                sodOrder_Status = order_Status,
                sodMinADA = minADA
            }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PolicyRedeemerCreateSellOfferType = PolicyRedeemerCreateSellOfferType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerCreateSellOfferType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerCreateSellOfferType

data PolicyRedeemerDeleteSellOfferType = PolicyRedeemerDeleteSellOfferType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerDeleteSellOfferType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerDeleteSellOfferType

data PolicyRedeemer
    = PolicyRedeemerCreateSellOfferID PolicyRedeemerCreateSellOfferType
    | PolicyRedeemerDeleteSellOfferID PolicyRedeemerDeleteSellOfferType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerCreateSellOfferID rmtx1 == PolicyRedeemerCreateSellOfferID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerDeleteSellOfferID rmtx1 == PolicyRedeemerDeleteSellOfferID rmtx2 = rmtx1 == rmtx2
    _ == _                                                                         = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerCreateSellOfferID, 1),
      ('PolicyRedeemerDeleteSellOfferID, 2)
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

newtype ValidatorRedeemerUpdateAskedCommissionRateType
    = ValidatorRedeemerUpdateAskedCommissionRateType { rucrNewCommissionRate :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateAskedCommissionRateType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rucrNewCommissionRate r1 == rucrNewCommissionRate r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateAskedCommissionRateType [('ValidatorRedeemerUpdateAskedCommissionRateType, 0)]


newtype ValidatorRedeemerUpdateMinADAType
    = ValidatorRedeemerUpdateMinADAType { rumaNewMinADA :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rumaNewMinADA r1 == rumaNewMinADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

data ValidatorRedeemerDepositType
    = ValidatorRedeemerDepositType
          { rdNewDeposit_FT  :: Integer
          , rdNewDeposit_ADA :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rdNewDeposit_FT r1 == rdNewDeposit_FT r2 &&
        rdNewDeposit_ADA r1 == rdNewDeposit_ADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

data ValidatorRedeemerWithdrawType
    = ValidatorRedeemerWithdrawType
          { rwNewWithdraw_FT  :: Integer
          , rwNewWithdraw_ADA :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwNewWithdraw_FT r1 == rwNewWithdraw_FT r2 &&
        rwNewWithdraw_ADA r1 == rwNewWithdraw_ADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]


data ValidatorRedeemerSwapFTxADAType
    = ValidatorRedeemerSwapFTxADAType
          { rsfxaAmount_FT        :: Integer
          , rsfxaAmount_ADA       :: Integer
          , rsfxaCommission_ADA   :: Integer
          , rsfxaOracle_Data      :: T.Oracle_Data
          , rsfxaOracle_Signature :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerSwapFTxADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rsfxaAmount_FT r1 == rsfxaAmount_FT r2 &&
        rsfxaAmount_ADA r1 == rsfxaAmount_ADA r2 &&
        rsfxaCommission_ADA r1 == rsfxaCommission_ADA r2 &&
        rsfxaOracle_Data r1 == rsfxaOracle_Data r2 &&
        rsfxaOracle_Signature r1 == rsfxaOracle_Signature r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerSwapFTxADAType [('ValidatorRedeemerSwapFTxADAType, 0)]

data ValidatorRedeemerSwapADAxFTType
    = ValidatorRedeemerSwapADAxFTType
          { rsaxfAmount_ADA       :: Integer
          , rsaxfAmount_FT        :: Integer
          , rsaxfCommission_FT    :: Integer
          , rsaxfOracle_Data      :: T.Oracle_Data
          , rsaxfOracle_Signature :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerSwapADAxFTType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rsaxfAmount_ADA r1 == rsaxfAmount_ADA r2 &&
        rsaxfAmount_FT r1 == rsaxfAmount_FT r2 &&
        rsaxfCommission_FT r1 == rsaxfCommission_FT r2 &&
        rsaxfOracle_Data r1 == rsaxfOracle_Data r2 &&
        rsaxfOracle_Signature r1 == rsaxfOracle_Signature r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerSwapADAxFTType [('ValidatorRedeemerSwapADAxFTType, 0)]

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

data ValidatorRedeemer
    = ValidatorRedeemerUpdateOrderStatus ValidatorRedeemerUpdateOrderStatusType
    | ValidatorRedeemerUpdateAskedCommissionRate ValidatorRedeemerUpdateAskedCommissionRateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerSwapFTxADA ValidatorRedeemerSwapFTxADAType
    | ValidatorRedeemerSwapADAxFT ValidatorRedeemerSwapADAxFTType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateOrderStatus rmf1 == ValidatorRedeemerUpdateOrderStatus rmf2                   = rmf1 == rmf2
    ValidatorRedeemerUpdateAskedCommissionRate rmcp1 == ValidatorRedeemerUpdateAskedCommissionRate rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2                           = rmcp1 == rmcp2
    ValidatorRedeemerDeposit rmcp1 == ValidatorRedeemerDeposit rmcp2                                     = rmcp1 == rmcp2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2                                   = rmcp1 == rmcp2
    ValidatorRedeemerSwapFTxADA rmcp1 == ValidatorRedeemerSwapFTxADA rmcp2                               = rmcp1 == rmcp2
    ValidatorRedeemerSwapADAxFT rmcp1 == ValidatorRedeemerSwapADAxFT rmcp2                               = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2                                       = rmcp1 == rmcp2
    _ == _                                                                                               = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateOrderStatus, 0),
      ('ValidatorRedeemerUpdateAskedCommissionRate, 1),
      ('ValidatorRedeemerUpdateMinADA, 2),
      ('ValidatorRedeemerDeposit, 3),
      ('ValidatorRedeemerWithdraw, 4),
      ('ValidatorRedeemerSwapFTxADA, 5),
      ('ValidatorRedeemerSwapADAxFT, 6),
      ('ValidatorRedeemerDelete, 7)
    ]


------------------------------------------------------------------------------
