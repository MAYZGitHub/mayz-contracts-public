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
module Protocol.Fund.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema  as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics         as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types        as T
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


data PolicyParams
    = PolicyParams
          { ppProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol
          , ppFundPolicy_TxOutRef :: LedgerApiV2.TxOutRef
          , ppFundValidator_Hash  :: LedgerApiV2.ValidatorHash
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        ppProtocolPolicyID_CS pp1 == ppProtocolPolicyID_CS pp2
            && ppFundValidator_Hash pp1 == ppFundValidator_Hash pp2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed
    ''PolicyParams
    [ ('PolicyParams, 0)
    ]

newtype ValidatorParams
    = ValidatorParams { vpProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2

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

data HoldingCreator
    = HoldingCreator
          { hcPaymentPKH :: T.WalletPaymentPKH
          , hcStakePKH   :: Maybe T.WalletPaymentPKH
          , hcMinADA     :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq HoldingCreator where
    {-# INLINEABLE (==) #-}
    mi1 == mi2 =
        hcPaymentPKH mi1 == hcPaymentPKH mi2
            && hcStakePKH mi1 == hcStakePKH mi2
            && hcMinADA mi1 == hcMinADA mi2

instance Ord HoldingCreator where
    {-# INLINEABLE compare #-}
    compare d1 d2 = compare (hcPaymentPKH d1) (hcPaymentPKH d2)

PlutusTx.makeIsDataIndexed
    ''HoldingCreator
    [ ('HoldingCreator, 0)
    ]

data FundDatumType
    = FundDatumType
          { fdFundFactoryVersion         :: Integer
          , fdFundPolicy_CS              :: T.CS
          , fdFundValidator_Hash         :: LedgerApiV2.ValidatorHash
          , fdFundHoldingPolicyID_CS     :: T.CS
          , fdFundHoldingValidator_Hash  :: LedgerApiV2.ValidatorHash
          , fdInvestUnitValidator_Hash   :: LedgerApiV2.ValidatorHash
          , fdAdmins                     :: [T.WalletPaymentPKH]
          , fdFundClassIndex             :: Integer
          , fdBeginAt                    :: LedgerApiV2.POSIXTime
          , fdDeadline                   :: LedgerApiV2.POSIXTime
          , fdClosedAt                   :: Maybe LedgerApiV2.POSIXTime
          , fdCommissionsPerYearInBPx1e3 :: Integer
          , fdHoldingsCount              :: Integer
          , fdHoldingsIndex              :: Integer
          , fdHoldingsCreators           :: [HoldingCreator]
          , fdMinADA                     :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq FundDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
           fdFundFactoryVersion ps1 == fdFundFactoryVersion ps2
            && fdFundPolicy_CS ps1 == fdFundPolicy_CS ps2
            && fdFundValidator_Hash ps1 == fdFundValidator_Hash ps2
            && fdFundHoldingPolicyID_CS ps1 == fdFundHoldingPolicyID_CS ps2
            && fdFundHoldingValidator_Hash ps1 == fdFundHoldingValidator_Hash ps2
            && fdInvestUnitValidator_Hash ps1 == fdInvestUnitValidator_Hash ps2
            && fdAdmins ps1 == fdAdmins ps2
            && fdFundClassIndex ps1 == fdFundClassIndex ps2
            && fdBeginAt ps1 == fdBeginAt ps2
            && fdDeadline ps1 == fdDeadline ps2
            && fdClosedAt ps1 == fdClosedAt ps2
            && fdCommissionsPerYearInBPx1e3 ps1 == fdCommissionsPerYearInBPx1e3 ps2
            && fdHoldingsCount ps1 == fdHoldingsCount ps2
            && fdHoldingsIndex ps1 == fdHoldingsIndex ps2
            && fdHoldingsCreators ps1 == fdHoldingsCreators ps2
            && fdMinADA ps1 == fdMinADA ps2

instance T.HasAdmins FundDatumType where
    {-# INLINEABLE getAdmins #-}
    getAdmins = fdAdmins

PlutusTx.makeIsDataIndexed
    ''FundDatumType
    [ ('FundDatumType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = FundDatum FundDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    FundDatum mps1 == FundDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('FundDatum, 0)
    ]

{-# INLINEABLE getFundDatumType #-}
getFundDatumType :: ValidatorDatum -> FundDatumType
getFundDatumType (FundDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkFundDatumType #-}
mkFundDatumType ::
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    LedgerApiV2.ValidatorHash ->
    [T.WalletPaymentPKH] ->
    Integer ->
    LedgerApiV2.POSIXTime ->
    LedgerApiV2.POSIXTime ->
    Maybe LedgerApiV2.POSIXTime ->
    Integer ->
    Integer ->
    Integer ->
    [HoldingCreator] ->
    Integer ->
    FundDatumType
mkFundDatumType
    fundpolicyID_CS
    fundValidator_Hash
    fundHoldingPolicyID_CS
    fundHoldingValidator_Hash
    investUnitValidator_Hash
    admins
    fundClassIndex
    beginAt
    deadline
    closedAt
    commission
    holdingsCount
    holdingsIndex
    holdingsCreators
    minADA =
        let !adminsOrdered = sort admins
            !holdingCreatorsOrdered = sort holdingsCreators
        in  FundDatumType
                {
                  fdFundPolicy_CS = fundpolicyID_CS,
                  fdFundValidator_Hash = fundValidator_Hash,
                  fdFundHoldingPolicyID_CS = fundHoldingPolicyID_CS,
                  fdFundHoldingValidator_Hash = fundHoldingValidator_Hash,
                  fdInvestUnitValidator_Hash = investUnitValidator_Hash,
                  fdAdmins = adminsOrdered,
                  fdFundClassIndex = fundClassIndex,
                  fdBeginAt = beginAt,
                  fdDeadline = deadline,
                  fdClosedAt = closedAt,
                  fdCommissionsPerYearInBPx1e3 = commission,
                  fdHoldingsCount = holdingsCount,
                  fdHoldingsIndex = holdingsIndex,
                  fdHoldingsCreators = holdingCreatorsOrdered,
                  fdMinADA = minADA,
                  fdFundFactoryVersion = T.fundFactoryVersion
                }

{-# INLINEABLE mkFundDatum #-}
mkFundDatum ::
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    T.CS ->
    LedgerApiV2.ValidatorHash ->
    LedgerApiV2.ValidatorHash ->
    [T.WalletPaymentPKH] ->
    Integer ->
    LedgerApiV2.POSIXTime ->
    LedgerApiV2.POSIXTime ->
    Maybe LedgerApiV2.POSIXTime ->
    Integer ->
    Integer ->
    Integer ->
    [HoldingCreator] ->
    Integer ->
    ValidatorDatum
mkFundDatum
    fundpolicyID_CS
    fundValidator_Hash
    fundHoldingPolicyID_CS
    fundHoldingValidator_Hash
    investUnitValidator_Hash
    admins
    fundClassIndex
    beginAt
    deadline
    closedAt
    commission
    holdingsCount
    holdingsIndex
    holdingsCreators
    minADA =
        FundDatum $
            mkFundDatumType
                fundpolicyID_CS
                fundValidator_Hash
                fundHoldingPolicyID_CS
                fundHoldingValidator_Hash
                investUnitValidator_Hash
                admins
                fundClassIndex
                beginAt
                deadline
                closedAt
                commission
                holdingsCount
                holdingsIndex
                holdingsCreators
                minADA

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintIDType [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [ ('PolicyRedeemerBurnIDType, 0)
    ]

data PolicyRedeemerMintFTType = PolicyRedeemerMintFTType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintFTType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintFTType [('PolicyRedeemerMintFTType, 0)]

data PolicyRedeemerBurnFTType = PolicyRedeemerBurnFTType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnFTType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''PolicyRedeemerBurnFTType [('PolicyRedeemerBurnFTType, 0)]

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    | PolicyRedeemerMintFT PolicyRedeemerMintFTType
    | PolicyRedeemerBurnFT PolicyRedeemerBurnFTType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerMintFT rmtx1 == PolicyRedeemerMintFT rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnFT rmtx1 == PolicyRedeemerBurnFT rmtx2 = rmtx1 == rmtx2
    _ == _                                                   = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0),
      ('PolicyRedeemerBurnID, 1),
      ('PolicyRedeemerMintFT, 2),
      ('PolicyRedeemerBurnFT, 3)
    ]

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

data ValidatorRedeemerFundHoldingAddType
    = ValidatorRedeemerFundHoldingAddType
          { rhaCreator                :: T.WalletPaymentPKH
          , rhaCreatorStakeCredential :: Maybe T.StakeCredentialPubKeyHash
          , rhaMinADA                 :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundHoldingAddType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rhaCreator r1 == rhaCreator r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundHoldingAddType
    [ ('ValidatorRedeemerFundHoldingAddType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerFundHoldingDeleteType = ValidatorRedeemerFundHoldingDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundHoldingDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundHoldingDeleteType
    [ ('ValidatorRedeemerFundHoldingDeleteType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorRedeemerFinishType
    = ValidatorRedeemerFinishType { rfDate :: LedgerApiV2.POSIXTime }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)
instance Eq ValidatorRedeemerFinishType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rfDate r1 == rfDate r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerFinishType [('ValidatorRedeemerFinishType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =   r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [ ('ValidatorRedeemerDeleteType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerFundHoldingAdd ValidatorRedeemerFundHoldingAddType
    | ValidatorRedeemerFundHoldingDelete ValidatorRedeemerFundHoldingDeleteType
    | ValidatorRedeemerFinish ValidatorRedeemerFinishType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerDatumUpdate rmf1 == ValidatorRedeemerDatumUpdate rmf2               = rmf1 == rmf2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2           = rmcp1 == rmcp2
    ValidatorRedeemerFundHoldingAdd rmcp1 == ValidatorRedeemerFundHoldingAdd rmcp2       = rmcp1 == rmcp2
    ValidatorRedeemerFundHoldingDelete rmcp1 == ValidatorRedeemerFundHoldingDelete rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerFinish rmcp1 == ValidatorRedeemerFinish rmcp2                       = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2                       = rmcp1 == rmcp2
    _ == _                                                                               = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [
        ('ValidatorRedeemerDatumUpdate, 0),
        ('ValidatorRedeemerUpdateMinADA, 1),
        ('ValidatorRedeemerFundHoldingAdd, 2),
        ('ValidatorRedeemerFundHoldingDelete, 3),
        ('ValidatorRedeemerFinish, 4),
        ('ValidatorRedeemerDelete, 5)
    ]

--------------------------------------------------------------------------------2
