{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.PABTypes where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson                  as DataAeson (FromJSON, ToJSON)
import qualified Data.ByteString             as DataByteString
import qualified Data.OpenApi.Schema         as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics                as GHCGenerics (Generic)
import qualified Ledger
import qualified Ledger.Address              as LedgerAddress
import qualified Ledger.Crypto               as Crypto
import qualified Ledger.Value                as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (unless)
import qualified Prelude                     as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers      as OnChainHelpers
import qualified Generic.Types               as T
import qualified Protocol.BuyOrder.Types     as BuyOrderT
import qualified Protocol.Constants          as T
import qualified Protocol.Delegation.Types   as DelegationT
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types         as FundT
import qualified Protocol.InvestUnit.Types   as InvestUnitT
import qualified Protocol.Protocol.Types     as ProtocolT
import qualified Protocol.Script.Types       as ScriptT
import qualified Protocol.SellOffer.Types    as SellOfferT
import qualified Protocol.Types              as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

data ProtocolPABParams
    = ProtocolPABParams
          { pppProtocolFactoryVersion    :: Integer
          , pppProtocolPolicyID_Params   :: ProtocolT.PolicyParams
          , pppProtocolPolicyID          :: LedgerApiV2.MintingPolicy
          , pppProtocolPolicyID_CS       :: LedgerApiV2.CurrencySymbol
          , pppProtocolValidator_Params  :: ProtocolT.ValidatorParams
          , pppProtocolValidator         :: LedgerApiV2.Validator
          , pppProtocolValidator_Hash    :: LedgerApiV2.ValidatorHash
          , pppProtocolValidator_Address :: LedgerAddress.Address
          , pppScriptPolicyID_Params     :: ScriptT.PolicyParams
          , pppScriptPolicyID            :: LedgerApiV2.MintingPolicy
          , pppScriptPolicyID_CS         :: LedgerApiV2.CurrencySymbol
          , pppScriptValidator_Params    :: ScriptT.ValidatorParams
          , pppScriptValidator           :: LedgerApiV2.Validator
          , pppScriptValidator_Hash      :: LedgerApiV2.ValidatorHash
          , pppScriptValidator_Address   :: LedgerAddress.Address
          , pppFundFactoryPABParams      :: [FundFactoryPABParams]
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data FundFactoryPABParams
    = FundFactoryPABParams
          { ffppFundFactoryVersion          :: Integer
          , ffppFundValidator_Params        :: FundT.ValidatorParams
          , ffppFundValidator               :: LedgerApiV2.Validator
          , ffppFundValidator_Hash          :: LedgerApiV2.ValidatorHash
          , ffppFundValidator_Address       :: LedgerAddress.Address
          , ffppInvestUnitValidator_Params  :: InvestUnitT.ValidatorParams
          , ffppInvestUnitValidator         :: LedgerApiV2.Validator
          , ffppInvestUnitValidator_Hash    :: LedgerApiV2.ValidatorHash
          , ffppInvestUnitValidator_Address :: LedgerAddress.Address
          , ffppFundPABParams               :: [FundPABParams]
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, Schema.ToSchema)

instance P.Show FundFactoryPABParams where
    show fp = "Fund Factory Params: " ++ P.show (ffppFundFactoryVersion  fp)

--------------------------------------------------------------------------------2

data FundPABParams
    = FundPABParams
          { fppFundFactoryVersion           :: Integer
          , fppFundPolicy_Params            :: FundT.PolicyParams
          , fppFundPolicy                   :: LedgerApiV2.MintingPolicy
          , fppFundPolicy_CS                :: LedgerApiV2.CurrencySymbol
          , fppFundValidator_Params         :: FundT.ValidatorParams
          , fppFundValidator                :: LedgerApiV2.Validator
          , fppFundValidator_Hash           :: LedgerApiV2.ValidatorHash
          , fppFundValidator_Address        :: LedgerAddress.Address
          , fppInvestUnitValidator_Params   :: InvestUnitT.ValidatorParams
          , fppInvestUnitValidator          :: LedgerApiV2.Validator
          , fppInvestUnitValidator_Hash     :: LedgerApiV2.ValidatorHash
          , fppInvestUnitValidator_Address  :: LedgerAddress.Address
          , fppFundHoldingPolicyID_Params   :: FundHoldingT.PolicyParams
          , fppFundHoldingPolicyID          :: LedgerApiV2.MintingPolicy
          , fppFundHoldingPolicyID_CS       :: LedgerApiV2.CurrencySymbol
          , fppFundHoldingValidator_Params  :: FundHoldingT.ValidatorParams
          , fppFundHoldingValidator         :: LedgerApiV2.Validator
          , fppFundHoldingValidator_Hash    :: LedgerApiV2.ValidatorHash
          , fppFundHoldingValidator_Address :: LedgerAddress.Address
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, Schema.ToSchema)

instance P.Show FundPABParams where
    show fp = "FundPABParams: " ++ P.show (fppFundPolicy_CS  fp)

--------------------------------------------------------------------------------2

data ProtocolDeployParams
    = ProtocolDeployParams
          { pdpProtocolFactoryVersion             :: Integer
          , pdpProtocolPolicyID_Params            :: ProtocolT.PolicyParams
          , pdpProtocolPolicyID_CborHex           :: P.String
          , pdpProtocolPolicyID_CS                :: LedgerApiV2.CurrencySymbol
          , pdpProtocolValidator_Params           :: ProtocolT.ValidatorParams
          , pdpProtocolValidator_Hash             :: LedgerApiV2.ValidatorHash
          , pdpProtocolValidator_CborHex          :: P.String
          , pdpProtocolValidator_AddressTestnet   :: P.String
          , pdpProtocolValidator_AddressMainnet   :: P.String
          , pdpScriptPolicyID_Params              :: ScriptT.PolicyParams
          , pdpScriptPolicyID_CborHex             :: P.String
          , pdpScriptPolicyID_CS                  :: LedgerApiV2.CurrencySymbol
          , pdpScriptValidator_Params             :: ScriptT.ValidatorParams
          , pdpScriptValidator_Hash               :: LedgerApiV2.ValidatorHash
          , pdpScriptValidator_CborHex            :: P.String
          , pdpScriptValidator_AddressTestnet     :: P.String
          , pdpScriptValidator_AddressMainnet     :: P.String
          , pdpSellOfferPolicyID_Params           :: SellOfferT.PolicyParams
          , pdpSellOfferPolicyID_CborHex          :: P.String
          , pdpSellOfferPolicyID_CS               :: LedgerApiV2.CurrencySymbol
          , pdpSellOfferValidator_Params          :: SellOfferT.ValidatorParams
          , pdpSellOfferValidator_Hash            :: LedgerApiV2.ValidatorHash
          , pdpSellOfferValidator_CborHex         :: P.String
          , pdpSellOfferValidator_AddressTestnet  :: P.String
          , pdpSellOfferValidator_AddressMainnet  :: P.String
          , pdpBuyOrderPolicyID_Params            :: BuyOrderT.PolicyParams
          , pdpBuyOrderPolicyID_CborHex           :: P.String
          , pdpBuyOrderPolicyID_CS                :: LedgerApiV2.CurrencySymbol
          , pdpBuyOrderValidator_Params           :: BuyOrderT.ValidatorParams
          , pdpBuyOrderValidator_Hash             :: LedgerApiV2.ValidatorHash
          , pdpBuyOrderValidator_CborHex          :: P.String
          , pdpBuyOrderValidator_AddressTestnet   :: P.String
          , pdpBuyOrderValidator_AddressMainnet   :: P.String
          , pdpDelegationPolicyID_Params          :: DelegationT.PolicyParams
          , pdpDelegationPolicyID_CborHex         :: P.String
          , pdpDelegationPolicyID_CS              :: LedgerApiV2.CurrencySymbol
          , pdpDelegationValidator_Params         :: DelegationT.ValidatorParams
          , pdpDelegationValidator_Hash           :: LedgerApiV2.ValidatorHash
          , pdpDelegationValidator_CborHex        :: P.String
          , pdpDelegationValidator_AddressTestnet :: P.String
          , pdpDelegationValidator_AddressMainnet :: P.String
          , pdpFundFactoryDeployParams            :: [FundFactoryDeployParams]
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
data FundFactoryDeployParams
    = FundFactoryDeployParams
          { ffdpFundFactoryVersion               :: Integer
          , ffdpFundPolicy_Pre_CborHex           :: P.String
          , ffdpFundValidator_Pre_CborHex        :: P.String
          , ffdpFundHoldingPolicyID_Pre_CborHex  :: P.String
          , ffdpFundHoldingValidator_Pre_CborHex :: P.String
          , ffdpInvestUnitValidator_Pre_CborHex  :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data FundDeployParams
    = FundDeployParams
          { fdpFundFactoryVersion                  :: Integer
          , fdpFundPolicy_Params                   :: FundT.PolicyParams
          , fdpFundPolicy_CborHex                  :: P.String
          , fdpFundPolicy_CS                       :: LedgerApiV2.CurrencySymbol
          , fdpFundValidator_Params                :: FundT.ValidatorParams
          , fdpFundValidator_Hash                  :: LedgerApiV2.ValidatorHash
          , fdpFundValidator_CborHex               :: P.String
          , fdpFundValidator_AddressTestnet        :: P.String
          , fdpFundValidator_AddressMainnet        :: P.String
          , fdpInvestUnitValidator_Params          :: InvestUnitT.ValidatorParams
          , fdpInvestUnitValidator_Hash            :: LedgerApiV2.ValidatorHash
          , fdpInvestUnitValidator_CborHex         :: P.String
          , fdpInvestUnitValidator_AddressTestnet  :: P.String
          , fdpInvestUnitValidator_AddressMainnet  :: P.String
          , fdpFundHoldingPolicyID_Params          :: FundHoldingT.PolicyParams
          , fdpFundHoldingPolicyID_CborHex         :: P.String
          , fdpFundHoldingPolicyID_CS              :: LedgerApiV2.CurrencySymbol
          , fdpFundHoldingValidator_Params         :: FundHoldingT.ValidatorParams
          , fdpFundHoldingValidator_Hash           :: LedgerApiV2.ValidatorHash
          , fdpFundHoldingValidator_CborHex        :: P.String
          , fdpFundHoldingValidator_AddressTestnet :: P.String
          , fdpFundHoldingValidator_AddressMainnet :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

newtype PABSplitUtxOParams
    = PABSplitUtxOParams { psupSplitAmount :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABMintFTParams
    = PABMintFTParams
          { pmfpPolicyNum          :: Integer
          , pmfpTokenNameBase      :: BuiltinByteString
          , pmfpDiifTokenNameCount :: Integer
          , pmfpAmount             :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABMintNFTParams
    = PABMintNFTParams
          { pmnpTokenNameBase      :: BuiltinByteString
          , pmnpDiifTokenNameCount :: Integer
          , pmnpAmount             :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
data PABMintFundTokensParams
    = PABMintFundTokensParams
          { pmftpProtocolPABParams :: ProtocolPABParams
          , pmftpFundPABParams     :: FundPABParams
          , pmftpAmount            :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PABProtocolPrepareParams
    = PABProtocolPrepareParams
          { ppppProtocolPABParams         :: ProtocolPABParams
          , ppppOraclePaymentPubKey       :: LedgerAddress.PaymentPubKey
          , ppppAdmins                    :: [T.WalletPaymentPKH]
          , ppppFundClasses               :: [ProtocolT.FundClass]
          , ppppFundLifeTime              :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
          , ppppRequiredMAYZForSellOffers :: Integer
          , ppppRequiredMAYZForBuyOrders  :: Integer
          , ppppCommissionFunds           :: ProtocolT.MinMaxDef Integer
          , ppppCommissionSellOffers      :: ProtocolT.MinMaxDef Integer
          , ppppCommissionBuyOrders       :: ProtocolT.MinMaxDef Integer
          , ppppShare_Protocol            :: Integer
          , ppppShare_MAYZ                :: Integer
          , ppppShare_FundAdmins          :: Integer
          , ppppMAYZWallets               :: [T.WalletPaymentPKH]
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABProtocolUpdateParams
    = PABProtocolUpdateParams
          { ppupProtocolPABParams         :: ProtocolPABParams
          , ppupOraclePaymentPubKey       :: LedgerAddress.PaymentPubKey
          , ppupAdmins                    :: [T.WalletPaymentPKH]
          , ppupFundClasses               :: [ProtocolT.FundClass]
          , ppupFundLifeTime              :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
          , ppupRequiredMAYZForSellOffers :: Integer
          , ppupRequiredMAYZForBuyOrders  :: Integer
          , ppupCommissionFunds           :: ProtocolT.MinMaxDef Integer
          , ppupCommissionSellOffers      :: ProtocolT.MinMaxDef Integer
          , ppupCommissionBuyOrders       :: ProtocolT.MinMaxDef Integer
          , ppupShare_Protocol            :: Integer
          , ppupShare_MAYZ                :: Integer
          , ppupShare_FundAdmins          :: Integer
          , ppupMAYZWallets               :: [T.WalletPaymentPKH]
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

newtype PABProtocolScriptAddParams
    = PABProtocolScriptAddParams { ppsapProtocolPABParams :: ProtocolPABParams }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

newtype PABProtocolScriptDeleteParams
    = PABProtocolScriptDeleteParams { ppsdpProtocolPABParams :: ProtocolPABParams }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)


--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PABFundPrepareParams
    = PABFundPrepareParams
          { pfppProtocolPABParams          :: ProtocolPABParams
          , pfppFundPABParams              :: FundPABParams
          , pfppAdmins                     :: [T.WalletPaymentPKH]
          , pfppFundClassIndex             :: Integer
          , pfppBeginAt                    :: LedgerApiV2.POSIXTime
          , pfppDeadline                   :: LedgerApiV2.POSIXTime
          , pfppClosedAt                   :: Maybe LedgerApiV2.POSIXTime
          , pfppCommissionsPerYearInBPx1e3 :: Integer
          , pfppInvestUnit                 :: T.InvestUnit
          , pfppInvestUnitPriceADA         :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundUpdateParams
    = PABFundUpdateParams
          { pfupProtocolPABParams :: ProtocolPABParams
          , pfupFundPABParams     :: FundPABParams
          , pfupAdmins            :: [T.WalletPaymentPKH]
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundScriptAddParams
    = PABFundScriptAddParams
          { pfsapProtocolPABParams :: ProtocolPABParams
          , pfsapFundPABParams     :: FundPABParams
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundScriptDeleteParams
    = PABFundScriptDeleteParams
          { pfsdpProtocolPABParams :: ProtocolPABParams
          , pfsdpFundPABParams     :: FundPABParams
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PABFundHoldingAddParams
    = PABFundHoldingAddParams
          { pfhapProtocolPABParams :: ProtocolPABParams
          , pfhapFundPABParams     :: FundPABParams
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundHoldingDeleteParams
    = PABFundHoldingDeleteParams
          { pfhdpProtocolPABParams   :: ProtocolPABParams
          , pfhdpFundPABParams       :: FundPABParams
          , pfhdpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundDepositParams
    = PABFundDepositParams
          { pfdpProtocolPABParams :: ProtocolPABParams
          , pfdpFundPABParams     :: FundPABParams
          , pfdpAmount            :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundWithdrawParams
    = PABFundWithdrawParams
          { pfwpProtocolPABParams :: ProtocolPABParams
          , pfwpFundPABParams     :: FundPABParams
          , pfwpAmount            :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundReIndexingParams
    = PABFundReIndexingParams
          { pfripProtocolPABParams   :: ProtocolPABParams
          , pfripFundPABParams       :: FundPABParams
          , pfripTokensToAdd         :: T.InvestUnit
          , pfripTokensToRemove      :: T.InvestUnit
          , pfripFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundUpdateOracleParams
    = PABFundUpdateOracleParams
          { pfuopProtocolPABParams :: ProtocolPABParams
          , pfuopFundPABParams     :: FundPABParams
          , pfuopPriceADA          :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundUpdateOracleReIdxParams
    = PABFundUpdateOracleReIdxParams
          { pfuoripProtocolPABParams   :: ProtocolPABParams
          , pfuoripFundPABParams       :: FundPABParams
          , pfuoripTokensWithPricesADA :: T.InvestUnit
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundCollect_Protocol_CommissionsParams
    = PABFundCollect_Protocol_CommissionsParams
          { pfwpcpProtocolPABParams   :: ProtocolPABParams
          , pfwpcpFundPABParams       :: FundPABParams
          , pfwpcpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
          , pfwpcpAmount              :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundCollect_MAYZ_CommissionsParams
    = PABFundCollect_MAYZ_CommissionsParams
          { pfwmcpProtocolPABParams   :: ProtocolPABParams
          , pfwmcpFundPABParams       :: FundPABParams
          , pfwmcpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
          , pfwmcpAmount              :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundCollect_FundAdmins_CommissionsParams
    = PABFundCollect_FundAdmins_CommissionsParams
          { pfwfcpProtocolPABParams   :: ProtocolPABParams
          , pfwfcpFundPABParams       :: FundPABParams
          , pfwfcpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
          , pfwfcpAmount              :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePOSIXTime :: LedgerApiV2.POSIXTime
examplePOSIXTime = 1658172331000

exampleTxOutRef :: LedgerApiV2.TxOutRef
exampleTxOutRef =
    LedgerApiV2.TxOutRef
        { LedgerApiV2.txOutRefId = "ed485b083eb5816c10c35a9d091d8af4cfdceef40c96578cae2b2266a8d976c9",
          LedgerApiV2.txOutRefIdx = 1
        }

exampleTxOutRef1 :: LedgerApiV2.TxOutRef
exampleTxOutRef1 =
    LedgerApiV2.TxOutRef
        { LedgerApiV2.txOutRefId = "ed485b083eb5816c10c35a9d091d8af4cfdceef40c96578cae2b2266a8d976c9",
          LedgerApiV2.txOutRefIdx = 2
        }

exampleAddress :: LedgerAddress.Address
exampleAddress = LedgerAddress.Address {LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e", LedgerApiV2.addressStakingCredential = Nothing}

exampleAddress1 :: LedgerAddress.Address
exampleAddress1 = LedgerAddress.Address {LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c1ff", LedgerApiV2.addressStakingCredential = Nothing}

exampleValidatorHash :: LedgerApiV2.ValidatorHash
exampleValidatorHash = LedgerApiV2.ValidatorHash "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c916ed"

exampleMkValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
exampleMkValidator _ _ _ = ()

exampleValidator :: LedgerApiV2.Validator
exampleValidator =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $$(PlutusTx.compile [||exampleMkValidator||])

exampleMkPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
exampleMkPolicy _ _ = ()

exampleMintingPolicy :: LedgerApiV2.MintingPolicy
exampleMintingPolicy =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $$(PlutusTx.compile [||exampleMkPolicy||])

exampleCS :: LedgerApiV2.CurrencySymbol
exampleCS = "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c916ed"

exampleTN :: LedgerApiV2.TokenName
exampleTN = "TN"

exampleNFT :: T.NFT
exampleNFT = LedgerValue.AssetClass (exampleCS, exampleTN)

exampleWalletPaymentPKH :: T.WalletPaymentPKH
exampleWalletPaymentPKH = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

exampleStakeCredentialPubKeyHash :: T.StakeCredentialPubKeyHash
exampleStakeCredentialPubKeyHash = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

examplePaymentPubKey :: Ledger.PaymentPubKey
examplePaymentPubKey = LedgerAddress.PaymentPubKey $ Crypto.PubKey $ LedgerApiV2.LedgerBytes $ Ledger.getPubKeyHash $ Ledger.unPaymentPubKeyHash $ Ledger.PaymentPubKeyHash exampleWalletPaymentPKH

exampleBBS :: LedgerApiV2.BuiltinByteString
exampleBBS = "aaccff"

exampleBool :: Bool
exampleBool = True

exampleInteger :: Integer
exampleInteger = 3_000_000

exampleString :: P.String
exampleString = "aaccff"

exampleValue :: LedgerApiV2.Value
exampleValue = LedgerApiV2.singleton exampleCS exampleTN exampleInteger

exampleFlattenValue ::  [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
exampleFlattenValue = OnChainHelpers.flattenValue exampleValue

exampleInvestUnit :: T.InvestUnit
exampleInvestUnit = T.InvestUnit { T.iuValues = OnChainHelpers.flattenValue exampleValue }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleProtocolValidatorParams :: ProtocolT.ValidatorParams
exampleProtocolValidatorParams =
    ProtocolT.ValidatorParams {
        ProtocolT.vpProtocolPolicyID_CS = exampleCS
    }

--------------------------------------------------------------------------------2

exampleFundPolicyParams :: FundT.PolicyParams
exampleFundPolicyParams =
    FundT.PolicyParams exampleCS exampleTxOutRef exampleValidatorHash

exampleFundValidatorParams :: FundT.ValidatorParams
exampleFundValidatorParams =
    FundT.ValidatorParams {
        FundT.vpProtocolPolicyID_CS = exampleCS
    }

exampleInvestUnitValidatorParams :: InvestUnitT.ValidatorParams
exampleInvestUnitValidatorParams =
    InvestUnitT.ValidatorParams {
        InvestUnitT.vpProtocolPolicyID_CS = exampleCS
    }

--------------------------------------------------------------------------------2

exampleProtocolPolicyParams :: ProtocolT.PolicyParams
exampleProtocolPolicyParams =
    ProtocolT.PolicyParams {
        ProtocolT.ppProtocolPolicyID_TxOutRef = exampleTxOutRef
    }

exampleScriptPolicyParams :: ScriptT.PolicyParams
exampleScriptPolicyParams =
    ScriptT.PolicyParams {
        ScriptT.ppProtocolPolicyID_CS = exampleCS
    }

exampleScriptValidatorParams :: ScriptT.ValidatorParams
exampleScriptValidatorParams =
    ScriptT.ValidatorParams exampleCS exampleCS

exampleFundHoldingPolicyParams :: FundHoldingT.PolicyParams
exampleFundHoldingPolicyParams =
    FundHoldingT.PolicyParams exampleCS


exampleFundHoldingValidatorParams :: FundHoldingT.ValidatorParams
exampleFundHoldingValidatorParams =
    FundHoldingT.ValidatorParams exampleCS exampleCS

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleProtocolPABParams :: ProtocolPABParams
exampleProtocolPABParams =
    ProtocolPABParams
        {
            pppProtocolFactoryVersion = T.protocolFactoryVersion,
            pppProtocolPolicyID_Params = exampleProtocolPolicyParams,
            pppProtocolPolicyID = exampleMintingPolicy,
            pppProtocolPolicyID_CS = exampleCS,
            pppProtocolValidator_Params = exampleProtocolValidatorParams,
            pppProtocolValidator = exampleValidator,
            pppProtocolValidator_Hash = exampleValidatorHash,
            pppProtocolValidator_Address = exampleAddress,
            pppScriptPolicyID_Params = exampleScriptPolicyParams,
            pppScriptPolicyID = exampleMintingPolicy,
            pppScriptPolicyID_CS = exampleCS,
            pppScriptValidator_Params = exampleScriptValidatorParams,
            pppScriptValidator = exampleValidator,
            pppScriptValidator_Hash = exampleValidatorHash,
            pppScriptValidator_Address = exampleAddress,
            pppFundFactoryPABParams = []
        }


--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleFundFactoryPABParams :: FundFactoryPABParams
exampleFundFactoryPABParams =
    FundFactoryPABParams
        {
            ffppFundFactoryVersion                = T.fundFactoryVersion,
            ffppFundValidator_Params = exampleFundValidatorParams,
            ffppFundValidator = exampleValidator,
            ffppFundValidator_Hash = exampleValidatorHash,
            ffppFundValidator_Address = exampleAddress,
            ffppInvestUnitValidator_Params = exampleInvestUnitValidatorParams,
            ffppInvestUnitValidator = exampleValidator,
            ffppInvestUnitValidator_Hash = exampleValidatorHash,
            ffppInvestUnitValidator_Address = exampleAddress,
            ffppFundPABParams              = [exampleFundPABParams]
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleFundPABParams :: FundPABParams
exampleFundPABParams =
    FundPABParams
        {
            fppFundFactoryVersion = T.fundFactoryVersion,
            fppFundPolicy_Params = exampleFundPolicyParams,
            fppFundPolicy = exampleMintingPolicy,
            fppFundPolicy_CS = exampleCS,
            fppFundValidator_Params = exampleFundValidatorParams,
            fppFundValidator = exampleValidator,
            fppFundValidator_Hash = exampleValidatorHash,
            fppFundValidator_Address = exampleAddress,
            fppInvestUnitValidator_Params = exampleInvestUnitValidatorParams,
            fppInvestUnitValidator = exampleValidator,
            fppInvestUnitValidator_Hash = exampleValidatorHash,
            fppInvestUnitValidator_Address = exampleAddress,
            fppFundHoldingPolicyID_Params = exampleFundHoldingPolicyParams,
            fppFundHoldingPolicyID = exampleMintingPolicy,
            fppFundHoldingPolicyID_CS = exampleCS,
            fppFundHoldingValidator_Params= exampleFundHoldingValidatorParams,
            fppFundHoldingValidator= exampleValidator,
            fppFundHoldingValidator_Hash= exampleValidatorHash,
            fppFundHoldingValidator_Address = exampleAddress
        }


--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABSplitUtxOParams :: PABSplitUtxOParams
examplePABSplitUtxOParams =
    PABSplitUtxOParams
        { psupSplitAmount = exampleInteger
        }

examplePABMintFTParams :: PABMintFTParams
examplePABMintFTParams =
    PABMintFTParams
        { pmfpPolicyNum = exampleInteger,
          pmfpTokenNameBase = exampleBBS,
          pmfpDiifTokenNameCount = exampleInteger,
          pmfpAmount = exampleInteger
        }

examplePABMintFundTokensParams :: PABMintFundTokensParams
examplePABMintFundTokensParams =
    PABMintFundTokensParams
        {
            pmftpProtocolPABParams  = exampleProtocolPABParams
          , pmftpFundPABParams    = exampleFundPABParams
          , pmftpAmount            = exampleInteger
        }

examplePABMintNFTParams :: PABMintNFTParams
examplePABMintNFTParams =
    PABMintNFTParams
        { pmnpTokenNameBase = exampleBBS,
          pmnpDiifTokenNameCount = exampleInteger,
          pmnpAmount = exampleInteger
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABProtocolPrepareParams :: PABProtocolPrepareParams
examplePABProtocolPrepareParams =
    PABProtocolPrepareParams
        { ppppProtocolPABParams = exampleProtocolPABParams,
          ppppOraclePaymentPubKey = examplePaymentPubKey,
          ppppAdmins = [exampleWalletPaymentPKH],
          ppppFundClasses = [ProtocolT.FundClass exampleInteger exampleInteger exampleInteger],
          ppppFundLifeTime = ProtocolT.mkMinMaxDef examplePOSIXTime examplePOSIXTime examplePOSIXTime,
          ppppRequiredMAYZForSellOffers = exampleInteger,
          ppppRequiredMAYZForBuyOrders = exampleInteger,
          ppppCommissionFunds = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger,
          ppppCommissionSellOffers = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger,
          ppppCommissionBuyOrders = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger,
          ppppShare_Protocol = exampleInteger,
          ppppShare_MAYZ = exampleInteger,
          ppppShare_FundAdmins = exampleInteger,
          ppppMAYZWallets = [exampleWalletPaymentPKH]
        }

examplePABProtocolUpdateParams :: PABProtocolUpdateParams
examplePABProtocolUpdateParams =
    PABProtocolUpdateParams
        {   ppupProtocolPABParams = exampleProtocolPABParams,
            ppupOraclePaymentPubKey = examplePaymentPubKey,
            ppupAdmins = [exampleWalletPaymentPKH],
            ppupFundClasses = [ProtocolT.FundClass exampleInteger exampleInteger exampleInteger],
            ppupFundLifeTime = ProtocolT.mkMinMaxDef examplePOSIXTime examplePOSIXTime examplePOSIXTime,
            ppupRequiredMAYZForSellOffers = exampleInteger,
            ppupRequiredMAYZForBuyOrders = exampleInteger,
            ppupCommissionFunds = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger,
            ppupCommissionSellOffers = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger,
            ppupCommissionBuyOrders = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger,
            ppupShare_Protocol = exampleInteger,
            ppupShare_MAYZ = exampleInteger,
            ppupShare_FundAdmins = exampleInteger,
          ppupMAYZWallets = [exampleWalletPaymentPKH]
        }

examplePABProtocolScriptAddParams :: PABProtocolScriptAddParams
examplePABProtocolScriptAddParams =
    PABProtocolScriptAddParams
        { ppsapProtocolPABParams = exampleProtocolPABParams
        }

examplePABProtocolScriptDeleteParams :: PABProtocolScriptDeleteParams
examplePABProtocolScriptDeleteParams =
    PABProtocolScriptDeleteParams
        { ppsdpProtocolPABParams = exampleProtocolPABParams
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABFundPrepareParams :: PABFundPrepareParams
examplePABFundPrepareParams =
    PABFundPrepareParams
        { pfppProtocolPABParams = exampleProtocolPABParams,
          pfppFundPABParams = exampleFundPABParams,
          pfppAdmins = [exampleWalletPaymentPKH],
          pfppFundClassIndex = exampleInteger,
          pfppBeginAt = examplePOSIXTime,
          pfppDeadline = examplePOSIXTime,
          pfppClosedAt = Just examplePOSIXTime,
          pfppCommissionsPerYearInBPx1e3 = exampleInteger,
          pfppInvestUnit = exampleInvestUnit,
          pfppInvestUnitPriceADA = exampleInteger
        }

examplePABFundUpdateParams :: PABFundUpdateParams
examplePABFundUpdateParams =
    PABFundUpdateParams
        { pfupProtocolPABParams = exampleProtocolPABParams,
            pfupFundPABParams = exampleFundPABParams,
          pfupAdmins = [exampleWalletPaymentPKH]
        }

examplePABFundScriptAddParams :: PABFundScriptAddParams
examplePABFundScriptAddParams =
    PABFundScriptAddParams
        {
            pfsapProtocolPABParams = exampleProtocolPABParams,
            pfsapFundPABParams = exampleFundPABParams
        }

examplePABFundScriptDeleteParams :: PABFundScriptDeleteParams
examplePABFundScriptDeleteParams =
    PABFundScriptDeleteParams
        {
            pfsdpProtocolPABParams = exampleProtocolPABParams,
            pfsdpFundPABParams = exampleFundPABParams
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABFundHoldingAddParams :: PABFundHoldingAddParams
examplePABFundHoldingAddParams =
    PABFundHoldingAddParams
        {
            pfhapProtocolPABParams= exampleProtocolPABParams,
            pfhapFundPABParams= exampleFundPABParams
        }

examplePABFundHoldingDeleteParams :: PABFundHoldingDeleteParams
examplePABFundHoldingDeleteParams =
    PABFundHoldingDeleteParams
        {
            pfhdpProtocolPABParams= exampleProtocolPABParams,
            pfhdpFundPABParams= exampleFundPABParams,
            pfhdpFundHoldingTxOutRef= exampleTxOutRef
        }

examplePABFundDepositParams :: PABFundDepositParams
examplePABFundDepositParams =
    PABFundDepositParams
        {
            pfdpProtocolPABParams= exampleProtocolPABParams,
            pfdpFundPABParams= exampleFundPABParams,
            pfdpAmount= exampleInteger
        }

examplePABFundWithdrawParams :: PABFundWithdrawParams
examplePABFundWithdrawParams =
    PABFundWithdrawParams
        {
            pfwpProtocolPABParams= exampleProtocolPABParams,
            pfwpFundPABParams= exampleFundPABParams,
            pfwpAmount= exampleInteger
        }

examplePABFundReIndexingParams :: PABFundReIndexingParams
examplePABFundReIndexingParams =
    PABFundReIndexingParams
        {
            pfripProtocolPABParams= exampleProtocolPABParams,
            pfripFundPABParams= exampleFundPABParams,
            pfripTokensToAdd = exampleInvestUnit,
            pfripTokensToRemove = exampleInvestUnit,
            pfripFundHoldingTxOutRef = exampleTxOutRef
        }

examplePABFundUpdateOracleParams :: PABFundUpdateOracleParams
examplePABFundUpdateOracleParams =
    PABFundUpdateOracleParams
        {
            pfuopProtocolPABParams= exampleProtocolPABParams,
            pfuopFundPABParams= exampleFundPABParams,
            pfuopPriceADA   = exampleInteger
        }

examplePABFundUpdateOracleReIdxParams :: PABFundUpdateOracleReIdxParams
examplePABFundUpdateOracleReIdxParams =
    PABFundUpdateOracleReIdxParams
        {
            pfuoripProtocolPABParams= exampleProtocolPABParams,
            pfuoripFundPABParams= exampleFundPABParams,
            pfuoripTokensWithPricesADA = exampleInvestUnit
        }


examplePABFundCollect_Protocol_CommissionsParams :: PABFundCollect_Protocol_CommissionsParams
examplePABFundCollect_Protocol_CommissionsParams =
    PABFundCollect_Protocol_CommissionsParams
        {
            pfwpcpProtocolPABParams= exampleProtocolPABParams,
            pfwpcpFundPABParams= exampleFundPABParams,
            pfwpcpFundHoldingTxOutRef   = exampleTxOutRef,
            pfwpcpAmount= exampleInteger
        }


examplePABFundCollect_MAYZ_CommissionsParams :: PABFundCollect_MAYZ_CommissionsParams
examplePABFundCollect_MAYZ_CommissionsParams =
    PABFundCollect_MAYZ_CommissionsParams
        {
            pfwmcpProtocolPABParams= exampleProtocolPABParams,
            pfwmcpFundPABParams= exampleFundPABParams,
            pfwmcpFundHoldingTxOutRef   = exampleTxOutRef,
            pfwmcpAmount= exampleInteger
        }


examplePABFundCollect_FundAdmins_CommissionsParams :: PABFundCollect_FundAdmins_CommissionsParams
examplePABFundCollect_FundAdmins_CommissionsParams =
    PABFundCollect_FundAdmins_CommissionsParams
        {
            pfwfcpProtocolPABParams= exampleProtocolPABParams,
            pfwfcpFundPABParams= exampleFundPABParams,
            pfwfcpFundHoldingTxOutRef   = exampleTxOutRef,
            pfwfcpAmount= exampleInteger
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

