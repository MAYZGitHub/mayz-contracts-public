{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Protocol.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema  as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics         as GHCGenerics (Generic)
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
import qualified Ledger.Address       as LedgerAddress
import qualified Protocol.Constants   as T
import qualified Protocol.Types       as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------22

newtype PolicyParams
    = PolicyParams { ppProtocolPolicyID_TxOutRef :: LedgerApiV2.TxOutRef }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 = ppProtocolPolicyID_TxOutRef pp1 == ppProtocolPolicyID_TxOutRef pp2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed
    ''PolicyParams
    [ ('PolicyParams, 0)
    ]

newtype ValidatorParams
    = ValidatorParams { vpProtocolPolicyID_CS :: T.CS }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 = vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed
    ''ValidatorParams
    [ ('ValidatorParams, 0)
    ]

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------22

data FundClass
    = FundClass
          { fcIndex        :: Integer
          , fcRequiredMAYZ :: Integer
          , fcMaxUI        :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq FundClass where
    {-# INLINEABLE (==) #-}
    mi1 == mi2 =
        fcIndex mi1 == fcIndex mi2
            && fcRequiredMAYZ mi1 == fcRequiredMAYZ mi2
            && fcMaxUI mi1 == fcMaxUI mi2

instance Ord FundClass where
    {-# INLINEABLE compare #-}
    compare fc1 fc2
        | fcIndex fc1 < fcIndex fc2 = LT
        | fcIndex fc1 == fcIndex fc2 && fcRequiredMAYZ fc1 < fcRequiredMAYZ fc2 = LT
        | fcIndex fc1 == fcIndex fc2 && fcRequiredMAYZ fc1 == fcRequiredMAYZ fc2  && fcMaxUI fc1 < fcMaxUI fc2 = LT
        | otherwise = GT

PlutusTx.makeIsDataIndexed
    ''FundClass
    [ ('FundClass, 0)
    ]

{-# INLINEABLE mkFundClass #-}
mkFundClass :: Integer -> Integer -> Integer -> FundClass
mkFundClass index requiredMAYZ maxUI = FundClass {fcIndex = index, fcRequiredMAYZ = requiredMAYZ, fcMaxUI = maxUI}

--------------------------------------------------------------------------------2

data MinMaxDef a
    = MinMaxDef
          { mmdMin :: a
          , mmdMax :: a
          , mmdDef :: a
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance (Eq a) => Eq (MinMaxDef a) where
    {-# INLINEABLE (==) #-}
    mi1 == mi2 =
        mmdMin mi1 == mmdMin mi2
            && mmdMax mi1 == mmdMax mi2
            && mmdDef mi1 == mmdDef mi2

instance DataOpenApiSchema.ToSchema (MinMaxDef Integer)
instance DataOpenApiSchema.ToSchema (MinMaxDef LedgerApiV2.POSIXTime)

PlutusTx.makeIsDataIndexed
    ''MinMaxDef
    [ ('MinMaxDef, 0)
    ]

{-# INLINEABLE mkMinMaxDef #-}
mkMinMaxDef :: a -> a -> a -> MinMaxDef a
mkMinMaxDef min' max' def' = MinMaxDef {mmdMin = min', mmdMax = max', mmdDef = def'}

--------------------------------------------------------------------------------2

data ProtocolDatumType
    = ProtocolDatumType
          { pdProtocolFactoryVersion    :: Integer
          , pdScriptPolicyID_CS         :: T.CS
          , pdScriptValidator_Hash      :: LedgerApiV2.ValidatorHash
          , pdOraclePaymentPubKey       :: LedgerAddress.PaymentPubKey
          , pdAdmins                    :: [T.WalletPaymentPKH]
          , pdFundClasses               :: [FundClass]
          , pdFundLifeTime              :: MinMaxDef LedgerApiV2.POSIXTime
          , pdRequiredMAYZForSellOffers :: Integer
          , pdRequiredMAYZForBuyOrders  :: Integer
          , pdCommissionFunds           :: MinMaxDef Integer
          , pdCommissionSellOffers      :: MinMaxDef Integer
          , pdCommissionBuyOrders       :: MinMaxDef Integer
          , pdShare_Protocol            :: Integer
          , pdShare_MAYZ                :: Integer
          , pdShare_FundAdmins          :: Integer
          , pdMAYZWallets               :: [T.WalletPaymentPKH]
          , pdMinADA                    :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ProtocolDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
            pdProtocolFactoryVersion ps1 == pdProtocolFactoryVersion ps2
            && pdScriptPolicyID_CS ps1 == pdScriptPolicyID_CS ps2
            && pdScriptValidator_Hash ps1 == pdScriptValidator_Hash ps2
            && pdOraclePaymentPubKey ps1 == pdOraclePaymentPubKey ps2
            && pdAdmins ps1 == pdAdmins ps2
            && pdFundClasses ps1 == pdFundClasses ps2
            && pdFundLifeTime ps1 == pdFundLifeTime ps2
            && pdRequiredMAYZForSellOffers ps1 == pdRequiredMAYZForSellOffers ps2
            && pdRequiredMAYZForBuyOrders ps1 == pdRequiredMAYZForBuyOrders ps2
            && pdCommissionFunds ps1 == pdCommissionFunds ps2
            && pdCommissionSellOffers ps1 == pdCommissionSellOffers ps2
            && pdCommissionBuyOrders ps1 == pdCommissionBuyOrders ps2
            && pdShare_Protocol ps1 == pdShare_Protocol ps2
            && pdShare_MAYZ ps1 == pdShare_MAYZ ps2
            && pdShare_FundAdmins ps1 == pdShare_FundAdmins ps2
            && pdMAYZWallets ps1 == pdMAYZWallets ps2
            && pdMinADA ps1 == pdMinADA ps2

instance T.HasAdmins ProtocolDatumType where
    {-# INLINEABLE getAdmins #-}
    getAdmins = pdAdmins

PlutusTx.makeIsDataIndexed
    ''ProtocolDatumType
    [ ('ProtocolDatumType, 0)
    ]

newtype ValidatorDatum
    = ProtocolDatum ProtocolDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    ProtocolDatum mps1 == ProtocolDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('ProtocolDatum, 0)
    ]

{-# INLINEABLE getProtocolDatumType #-}
getProtocolDatumType :: ValidatorDatum -> ProtocolDatumType
getProtocolDatumType (ProtocolDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkProtocolDatumType #-}
mkProtocolDatumType ::
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    LedgerAddress.PaymentPubKey->
    [T.WalletPaymentPKH] ->
    [FundClass] ->
    MinMaxDef LedgerApiV2.POSIXTime ->
    Integer ->
    Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    Integer ->
    Integer ->
    Integer ->
    [T.WalletPaymentPKH] ->
    Integer ->
    ProtocolDatumType
mkProtocolDatumType
    scriptPolicyID_CS
    scriptValidator_Hash
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
    minADA =
        let
            !adminsOrdered = sort admins
            !fundClassesOrdered = sort fundClasses
            !mayzWalletsOrdered = sort mayzWallets
        in  ProtocolDatumType
                {
                  pdProtocolFactoryVersion = T.protocolFactoryVersion,
                  pdScriptPolicyID_CS = scriptPolicyID_CS,
                  pdScriptValidator_Hash = scriptValidator_Hash,
                  pdOraclePaymentPubKey = oraclePaymentPubKey,
                  pdAdmins = adminsOrdered,
                  pdFundClasses = fundClassesOrdered,
                  pdFundLifeTime = fundLifeTime,
                  pdRequiredMAYZForSellOffers = requiredMAYZForSellOffers,
                  pdRequiredMAYZForBuyOrders = requiredMAYZForBuyOrders,
                  pdCommissionFunds = commissionFunds,
                  pdCommissionSellOffers = commissionSellOffers,
                  pdCommissionBuyOrders = commissionBuyOrders,
                  pdShare_Protocol = share_Protocol,
                  pdShare_MAYZ = share_MAYZ,
                  pdShare_FundAdmins = share_FundAdmins,
                  pdMAYZWallets = mayzWalletsOrdered,
                  pdMinADA = minADA
                }

{-# INLINEABLE mkProtocolDatum #-}
mkProtocolDatum ::
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    LedgerAddress.PaymentPubKey ->
    [T.WalletPaymentPKH] ->
    [FundClass] ->
    MinMaxDef LedgerApiV2.POSIXTime ->
    Integer ->
    Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    MinMaxDef Integer ->
    Integer ->
    Integer ->
    Integer ->
    [T.WalletPaymentPKH] ->
    Integer ->
    ValidatorDatum
mkProtocolDatum
    scriptPolicyID_CS
    scriptValidator_Hash
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
    minADA =
        ProtocolDatum $
            mkProtocolDatumType
                scriptPolicyID_CS
                scriptValidator_Hash
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
                minADA

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDatumUpdateType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDatumUpdateType
    [ ('ValidatorRedeemerDatumUpdateType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorRedeemerUpdateMinADAType
    = ValidatorRedeemerUpdateMinADAType { rumaNewMinADA :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rumaNewMinADA r1 == rumaNewMinADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerDatumUpdate rmf1 == ValidatorRedeemerDatumUpdate rmf2   = rmf1 == rmf2
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2 = rmf1 == rmf2
    _ == _                                                                   = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerDatumUpdate, 0),
        ('ValidatorRedeemerUpdateMinADA, 1)
    ]

--------------------------------------------------------------------------------2
