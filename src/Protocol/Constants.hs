{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Constants where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2`
import PlutusTx.Prelude
import qualified Data.ByteString                                 as DataByteString

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2
import qualified Generic.Types    as T
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Generic.OffChainHelpers as OffChainHelpers
--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

sellOffer_Status_Open  :: Integer
sellOffer_Status_Open  = 1

sellOffer_Status_Closed  :: Integer
sellOffer_Status_Closed  = 2

buyOrder_Status_Open  :: Integer
buyOrder_Status_Open  = 1

buyOrder_Status_Closed  :: Integer
buyOrder_Status_Closed  = 2

--------------------------------------------------------------------------------2

protocolID_TN :: T.TN
protocolID_TN = LedgerApiV2.TokenName "ProtocolID"

fundID_TN :: T.TN
fundID_TN = LedgerApiV2.TokenName "FundID"

fundHoldingID_TN_basename :: BuiltinByteString
fundHoldingID_TN_basename = "FundHoldingID"

investUnitID_TN :: T.TN
investUnitID_TN = LedgerApiV2.TokenName "IUID"

fundFT_TN :: T.TN
fundFT_TN = LedgerApiV2.TokenName "FT"

holdingID_TN :: T.TN
holdingID_TN = LedgerApiV2.TokenName "HoldingID"

sellOfferID_TN :: T.TN
sellOfferID_TN = LedgerApiV2.TokenName "SellOfferID"

buyOrderID_TN :: T.TN
buyOrderID_TN = LedgerApiV2.TokenName "BuyOrderID"

delegationID_TN :: T.TN
delegationID_TN = LedgerApiV2.TokenName "DelegationID"



--------------------------------------------------------------------------------2

coreTeamWallets :: [T.WalletPaymentPKH]
coreTeamWallets = ["a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2", "80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7"]

--------------------------------------------------------------------------------2

tokenMAYZ_CS :: LedgerApiV2.CurrencySymbol
-- tokenMAYZ_CS =  "7288521b8663f07fd6dabe5bdb4a951f57bc24d58fd4b5e92f3ab301" -- for PAB Simualtor ID of policy index 10 when using the mint CLI
tokenMAYZ_CS =  "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" -- for TESTNET usign rats token
-- tokenMAYZ_CS =  LedgerApiV2.CurrencySymbol $ OffChainHelpers.builtinByteStringFromHexString "7288521b8663f07fd6dabe5bdb4a951f57bc24d58fd4b5e92f3ab301" -- mainnet

tokenMAYZ_TN :: T.TN
-- tokenMAYZ_TN = LedgerApiV2.TokenName "MAYZ" -- for PAB Simualtor and mainnet
tokenMAYZ_TN = LedgerApiV2.TokenName "Rats" -- for TESTNET usign rats token

--------------------------------------------------------------------------------2

protocolFactoryVersion :: Integer
protocolFactoryVersion = 2 

fundFactoryVersion :: Integer
fundFactoryVersion = 2 

--------------------------------------------------------------------------------2

maxDeposit:: Integer
maxDeposit= 10_000_000_000_000_000 

--------------------------------------------------------------------------------2

oracleData_Valid_Time :: LedgerApiV2.POSIXTime
oracleData_Valid_Time = 300_000 -- 5 * 60 * 1000 = 5 minutos

--------------------------------------------------------------------------------2

oracleWallet_Seed :: DataByteString.ByteString
oracleWallet_Seed = "he hi he ds fd gg ge eew rer trt erw rwerwe trter gfgdf gfdgdf rtet trtre treter ghfhgf treter gfdgdf tretre gfdgdf tretre"


--------------------------------------------------------------------------------2
