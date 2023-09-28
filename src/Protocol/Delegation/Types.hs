{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
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
module Protocol.Delegation.Types where

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
          { ppProtocolPolicyID_CS       :: T.CS
          , ppDelegation_Validator_Hash :: LedgerApiV2.ValidatorHash
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 = ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2 &&
        ppDelegation_Validator_Hash p1 == ppDelegation_Validator_Hash p2

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

data Delegation_DatumType
    = Delegation_DatumType
          { ddDelegationPolicyID_CS :: T.CS
          , ddFundPolicy_CS         :: T.CS
          , ddDelegatorPaymentPKH   :: T.WalletPaymentPKH
          , ddDelegatorStakePKH     :: Maybe T.WalletPaymentPKH
          , ddDelegated_Mayz        :: Integer
          , ddMinADA                :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq Delegation_DatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
            ddDelegationPolicyID_CS sd1 == ddDelegationPolicyID_CS sd2
            && ddFundPolicy_CS sd1 == ddFundPolicy_CS sd2
            && ddDelegatorPaymentPKH sd1 == ddDelegatorPaymentPKH sd2
            && ddDelegatorStakePKH sd1 == ddDelegatorStakePKH sd2
            && ddDelegated_Mayz sd1 == ddDelegated_Mayz sd2
            && ddMinADA sd1 == ddMinADA sd2

PlutusTx.makeIsDataIndexed ''Delegation_DatumType [('Delegation_DatumType, 0)]

newtype ValidatorDatum
    = Delegation_Datum Delegation_DatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    Delegation_Datum sd1 == Delegation_Datum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('Delegation_Datum, 0)]

{-# INLINEABLE getDelegation_DatumType #-}
getDelegation_DatumType :: ValidatorDatum -> Delegation_DatumType
getDelegation_DatumType (Delegation_Datum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkDelegation_Datum #-}
mkDelegation_Datum :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer -> Integer -> ValidatorDatum
mkDelegation_Datum
    delegationPolicyID_CS
    fundPolicy_CS
    delegatorPaymentPKH
    delegatorStakePKH
    delegated_Mayz
    minADA
    =
        Delegation_Datum $
            mkDelegation_DatumType
                delegationPolicyID_CS
                fundPolicy_CS
                delegatorPaymentPKH
                delegatorStakePKH
                delegated_Mayz
                minADA

{-# INLINEABLE mkDelegation_DatumType #-}
mkDelegation_DatumType :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> Integer ->  Integer  -> Delegation_DatumType
mkDelegation_DatumType
    delegationPolicyID_CS
    fundPolicy_CS
    delegatorPaymentPKH
    delegatorStakePKH
    delegated_Mayz
    minADA
        =
        Delegation_DatumType
            {
                ddDelegationPolicyID_CS = delegationPolicyID_CS,
                ddFundPolicy_CS = fundPolicy_CS,
                ddDelegatorPaymentPKH = delegatorPaymentPKH,
                ddDelegatorStakePKH = delegatorStakePKH,
                ddDelegated_Mayz = delegated_Mayz,
                ddMinADA = minADA
            }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PolicyRedeemerCreateDelegationType = PolicyRedeemerCreateDelegationType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerCreateDelegationType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerCreateDelegationType

data PolicyRedeemerDeleteDelegationType = PolicyRedeemerDeleteDelegationType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerDeleteDelegationType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerDeleteDelegationType

data PolicyRedeemer
    = PolicyRedeemerCreateDelegationID PolicyRedeemerCreateDelegationType
    | PolicyRedeemerDeleteDelegationID PolicyRedeemerDeleteDelegationType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerCreateDelegationID rmtx1 == PolicyRedeemerCreateDelegationID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerDeleteDelegationID rmtx1 == PolicyRedeemerDeleteDelegationID rmtx2 = rmtx1 == rmtx2
    _ == _                                                                           = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerCreateDelegationID, 1),
      ('PolicyRedeemerDeleteDelegationID, 2)
    ]

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2


newtype ValidatorRedeemerDepositType
    = ValidatorRedeemerDepositType { vrdDelegated_Mayz_Change :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = vrdDelegated_Mayz_Change r1 == vrdDelegated_Mayz_Change r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

newtype ValidatorRedeemerWithdrawType
    = ValidatorRedeemerWithdrawType { vrdwDelegated_Mayz_Change :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = vrdwDelegated_Mayz_Change r1 == vrdwDelegated_Mayz_Change r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]


data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    (==) :: ValidatorRedeemerDeleteType -> ValidatorRedeemerDeleteType -> Bool
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

data ValidatorRedeemer
    = ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerDeposit rmcp1 == ValidatorRedeemerDeposit rmcp2   = rmcp1 == rmcp2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2     = rmcp1 == rmcp2
    _ == _                                                             = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [
      ('ValidatorRedeemerDeposit, 0),
      ('ValidatorRedeemerWithdraw, 1),
      ('ValidatorRedeemerDelete, 2)

    ]


------------------------------------------------------------------------------
