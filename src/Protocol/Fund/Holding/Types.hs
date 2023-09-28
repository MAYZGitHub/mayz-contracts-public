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
module Protocol.Fund.Holding.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson                as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema       as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics              as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                   as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Types             as T
import qualified Protocol.Constants        as T
import qualified Protocol.InvestUnit.Types as InvestUnitT
import qualified Protocol.Types            as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------22

newtype PolicyParams
    = PolicyParams { ppFundPolicy_CS :: LedgerApiV2.CurrencySymbol }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
         ppFundPolicy_CS pp1 == ppFundPolicy_CS pp2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed
    ''PolicyParams
    [ ('PolicyParams, 0)
    ]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol
          , vpFundPolicy_CS       :: LedgerApiV2.CurrencySymbol
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2
            &&
        vpFundPolicy_CS pp1 == vpFundPolicy_CS pp2

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

data FundHoldingDatumType
    = FundHoldingDatumType
          { hdFundHolding_Index                              :: Integer
          , hdSubtotal_FT_Minted_Accumulated                 :: Integer
          , hdSubtotal_FT_Minted                             :: Integer
          , hdSubtotal_FT_Circulation                        :: Integer
          , hdSubtotal_FT_ForComission                       :: Integer
          , hdSubtotal_FT_ForComission_Acumulated            :: Integer
          , hdSubtotal_Commissions_RatePerMonth_Numerator1e6 :: Integer
          , hdSubtotal_Collected_Commissions_Protocol        :: Integer
          , hdSubtotal_Collected_Commissions_MAYZ            :: Integer
          , hdSubtotal_Collected_Commissions_FundAdmins      :: Integer
          , hdMinADA                                         :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq FundHoldingDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        hdFundHolding_Index ps1 == hdFundHolding_Index ps2
            && hdSubtotal_FT_Minted_Accumulated ps1 == hdSubtotal_FT_Minted_Accumulated ps2
            && hdSubtotal_FT_Minted ps1 == hdSubtotal_FT_Minted ps2
            && hdSubtotal_FT_Circulation ps1 == hdSubtotal_FT_Circulation ps2
            && hdSubtotal_FT_ForComission ps1 == hdSubtotal_FT_ForComission ps2
            && hdSubtotal_FT_ForComission_Acumulated ps1 == hdSubtotal_FT_ForComission_Acumulated ps2
            && hdSubtotal_Commissions_RatePerMonth_Numerator1e6 ps1 == hdSubtotal_Commissions_RatePerMonth_Numerator1e6 ps2
            && hdSubtotal_Collected_Commissions_Protocol ps1 == hdSubtotal_Collected_Commissions_Protocol ps2
            && hdSubtotal_Collected_Commissions_MAYZ ps1 == hdSubtotal_Collected_Commissions_MAYZ ps2
            && hdSubtotal_Collected_Commissions_FundAdmins ps1 == hdSubtotal_Collected_Commissions_FundAdmins ps2
            && hdMinADA ps1 == hdMinADA ps2

PlutusTx.makeIsDataIndexed
    ''FundHoldingDatumType
    [ ('FundHoldingDatumType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = FundHoldingDatum FundHoldingDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    FundHoldingDatum mps1 == FundHoldingDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('FundHoldingDatum, 0)
    ]

{-# INLINEABLE getFundHoldingDatumType #-}
getFundHoldingDatumType :: ValidatorDatum -> FundHoldingDatumType
getFundHoldingDatumType (FundHoldingDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkFundHoldingDatumType #-}
mkFundHoldingDatumType :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> FundHoldingDatumType
mkFundHoldingDatumType
    holdingIndex
    subtotal_FT_AcumDeposits
    subtotal_FT_Minted
    subtotal_FT_Circulation
    subtotal_FT_ForComission
    subtotal_Commissions_Payed
    subtotal_Commissions_RatePerMonth_Numerator1e6
    subtotal_Collected_Commissions_Protocolo
    subtotal_Collected_Commissions_MAYZ
    subtotal_Collected_Commissions_FundAdmins
    minADA =
        FundHoldingDatumType {
            hdFundHolding_Index = holdingIndex,
            hdSubtotal_FT_Minted_Accumulated = subtotal_FT_AcumDeposits,
            hdSubtotal_FT_Minted = subtotal_FT_Minted,
            hdSubtotal_FT_Circulation = subtotal_FT_Circulation,
            hdSubtotal_FT_ForComission = subtotal_FT_ForComission,
            hdSubtotal_FT_ForComission_Acumulated = subtotal_Commissions_Payed,
            hdSubtotal_Commissions_RatePerMonth_Numerator1e6 = subtotal_Commissions_RatePerMonth_Numerator1e6,
            hdSubtotal_Collected_Commissions_Protocol = subtotal_Collected_Commissions_Protocolo,
            hdSubtotal_Collected_Commissions_MAYZ = subtotal_Collected_Commissions_MAYZ,
            hdSubtotal_Collected_Commissions_FundAdmins = subtotal_Collected_Commissions_FundAdmins,
            hdMinADA = minADA
            }


{-# INLINEABLE mkFundHoldingDatum #-}
mkFundHoldingDatum :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ValidatorDatum
mkFundHoldingDatum holdingIndex
    subtotal_FT_AcumDeposits
    subtotal_FT_Minted
    subtotal_FT_Circulation
    subtotal_FT_ForComission
    subtotal_Commissions_Payed
    subtotal_Commissions_RatePerMonth_Numerator1e6
    subtotal_Collected_Commissions_Protocolo
    subtotal_Collected_Commissions_MAYZ
    subtotal_Collected_Commissions_FundAdmins
    minADA =
        FundHoldingDatum $
            mkFundHoldingDatumType
                holdingIndex
                subtotal_FT_AcumDeposits
                subtotal_FT_Minted
                subtotal_FT_Circulation
                subtotal_FT_ForComission
                subtotal_Commissions_Payed
                subtotal_Commissions_RatePerMonth_Numerator1e6
                subtotal_Collected_Commissions_Protocolo
                subtotal_Collected_Commissions_MAYZ
                subtotal_Collected_Commissions_FundAdmins
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

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _                                                   = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0),
      ('PolicyRedeemerBurnID, 1)
    ]

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

data ValidatorRedeemerDepositType
    = ValidatorRedeemerDepositType
          { rdDate   :: LedgerApiV2.POSIXTime
          , rdAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
       rdDate r1 == rdDate r2
       && rdAmount r1 == rdAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDepositType
    [ ('ValidatorRedeemerDepositType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerWithdrawType
    = ValidatorRedeemerWithdrawType
          { rwDate   :: LedgerApiV2.POSIXTime
          , rwAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
       rwDate r1 == rwDate r2 &&
        rwAmount r1 == rwAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerWithdrawType
    [ ('ValidatorRedeemerWithdrawType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerCollect_Protocol_CommissionsType
    = ValidatorRedeemerCollect_Protocol_CommissionsType
          { rwpcDate   :: LedgerApiV2.POSIXTime
          , rwpcAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollect_Protocol_CommissionsType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwpcDate r1 == rwpcDate r2 && rwpcAmount r1 == rwpcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Protocol_CommissionsType
    [ ('ValidatorRedeemerCollect_Protocol_CommissionsType, 0)
    ]

--------------------------------------------------------------------------------2
data ValidatorRedeemerCollect_MAYZ_CommissionsType
    = ValidatorRedeemerCollect_MAYZ_CommissionsType
          { rwmcDate   :: LedgerApiV2.POSIXTime
          , rwmcAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollect_MAYZ_CommissionsType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwmcDate r1 == rwmcDate r2 && rwmcAmount r1 == rwmcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_MAYZ_CommissionsType
    [ ('ValidatorRedeemerCollect_MAYZ_CommissionsType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerCollect_FundAdmins_CommissionsType
    = ValidatorRedeemerCollect_FundAdmins_CommissionsType
          { rwfcDate   :: LedgerApiV2.POSIXTime
          , rwfcAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollect_FundAdmins_CommissionsType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwfcDate r1 == rwfcDate r2 && rwfcAmount r1 == rwfcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_FundAdmins_CommissionsType
    [ ('ValidatorRedeemerCollect_FundAdmins_CommissionsType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerReIndexingType
    = ValidatorRedeemerReIndexingType
          { rriTokensToAdd    :: T.InvestUnit
          , rriTokensToRemove :: T.InvestUnit
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerReIndexingType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rriTokensToAdd r1 == rriTokensToAdd r2          && rriTokensToRemove r1 == rriTokensToRemove r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReIndexingType
    [ ('ValidatorRedeemerReIndexingType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerDeleteType
    = ValidatorRedeemerDeleteType
    -- { rhdAdmin :: T.WalletPaymentPKH }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =   r1 == r2
        -- rhdAdmin r1 == rhdAdmin r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [ ('ValidatorRedeemerDeleteType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerCollect_Protocol_Commissions ValidatorRedeemerCollect_Protocol_CommissionsType
    | ValidatorRedeemerCollect_MAYZ_Commissions ValidatorRedeemerCollect_MAYZ_CommissionsType
    | ValidatorRedeemerCollect_FundAdmins_Commissions ValidatorRedeemerCollect_FundAdmins_CommissionsType
    | ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2                                       = rmf1 == rmf2
    ValidatorRedeemerDeposit rmf1 == ValidatorRedeemerDeposit rmf2                                                 = rmf1 == rmf2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2                                             = rmcp1 == rmcp2
    ValidatorRedeemerCollect_Protocol_Commissions rmcp1 == ValidatorRedeemerCollect_Protocol_Commissions rmcp2     = rmcp1 == rmcp2
    ValidatorRedeemerCollect_MAYZ_Commissions rmcp1 == ValidatorRedeemerCollect_MAYZ_Commissions rmcp2             = rmcp1 == rmcp2
    ValidatorRedeemerCollect_FundAdmins_Commissions rmcp1 == ValidatorRedeemerCollect_FundAdmins_Commissions rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerReIndexing rmcp1 == ValidatorRedeemerReIndexing rmcp2                                         = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2                                                 = rmcp1 == rmcp2
    _ == _                                                                                                         = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [
        ('ValidatorRedeemerUpdateMinADA, 0),
        ('ValidatorRedeemerDeposit, 1),
        ('ValidatorRedeemerWithdraw, 2),
        ('ValidatorRedeemerCollect_Protocol_Commissions, 3),
        ('ValidatorRedeemerCollect_MAYZ_Commissions, 4),
        ('ValidatorRedeemerCollect_FundAdmins_Commissions, 5),
        ('ValidatorRedeemerReIndexing, 6),
        ('ValidatorRedeemerDelete, 7)
    ]

--------------------------------------------------------------------------------2
