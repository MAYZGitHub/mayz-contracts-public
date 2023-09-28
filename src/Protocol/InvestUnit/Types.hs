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
module Protocol.InvestUnit.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson             as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema    as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics           as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types          as T
import qualified Ledger
import qualified PlutusTx.Builtins      as TxBuiltins
import qualified Protocol.Types         as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------22

newtype ValidatorParams
    = ValidatorParams { vpProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        -- vpPolicyID_CS pp1 == vpPolicyID_CS pp2
        --  &&
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
--------------------------------------------------------------------------------2

data InvestUnitDatumType
    = InvestUnitDatumType
          { iudFundPolicy_CS :: T.CS
          , iudInvestUnit      :: T.InvestUnit
          , iudMinADA          :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq InvestUnitDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        iudFundPolicy_CS ps1 == iudFundPolicy_CS ps2
            && iudInvestUnit ps1 == iudInvestUnit ps2
            && iudMinADA ps1 == iudMinADA ps2


PlutusTx.makeIsDataIndexed
    ''InvestUnitDatumType
    [ ('InvestUnitDatumType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = InvestUnitDatum InvestUnitDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    InvestUnitDatum mps1 == InvestUnitDatum mps2 = mps1 == mps2



PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('InvestUnitDatum, 0)
    ]

{-# INLINEABLE getInvestUnitDatumType #-}
getInvestUnitDatumType :: ValidatorDatum -> InvestUnitDatumType
getInvestUnitDatumType (InvestUnitDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkInvestUnitDatumType #-}
mkInvestUnitDatumType :: T.CS -> T.InvestUnit -> Integer -> InvestUnitDatumType
mkInvestUnitDatumType  fundPolicy_CS investUnit minADA =
    InvestUnitDatumType
        { iudFundPolicy_CS = fundPolicy_CS
        , iudInvestUnit = investUnit
        , iudMinADA = minADA
        }

{-# INLINEABLE mkInvestUnitDatum #-}
mkInvestUnitDatum :: T.CS -> T.InvestUnit -> Integer -> ValidatorDatum
mkInvestUnitDatum fundPolicy_CS investUnit minADA =
    InvestUnitDatum $ mkInvestUnitDatumType fundPolicy_CS investUnit minADA

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

newtype ValidatorRedeemerUpdateMinADAType
    = ValidatorRedeemerUpdateMinADAType { rumaNewMinADA :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rumaNewMinADA r1 == rumaNewMinADA r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerReIndexingType
    = ValidatorRedeemerReIndexingType
          { riuriTokensToAdd      :: T.InvestUnit
          , riuriTokensToRemove   :: T.InvestUnit
          , riuriOracleReIdx_Data :: T.OracleReIdx_Data
          , riuriOracleSignature  :: Ledger.Signature
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerReIndexingType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        riuriTokensToAdd r1 == riuriTokensToAdd r2
        && riuriTokensToRemove r1 == riuriTokensToRemove r2
        && riuriOracleReIdx_Data r1 == riuriOracleReIdx_Data r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReIndexingType
    [ ('ValidatorRedeemerReIndexingType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2 = rmf1 == rmf2
    ValidatorRedeemerReIndexing rmf1 == ValidatorRedeemerReIndexing rmf2     = rmf1 == rmf2
    _ == _                                                                   = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerReIndexing, 0),
        ('ValidatorRedeemerUpdateMinADA, 1)
    ]

--------------------------------------------------------------------------------2


