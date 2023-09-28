{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Generic.Constants where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Cardano.Node.Emulator.Params as CardanoNodeEmulatorParams
import qualified Ledger
import qualified Plutus.V2.Ledger.Api         as LedgerApiV2

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- FOR CONFIGURATION:

validTimeRange :: LedgerApiV2.POSIXTime
validTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos

--------------------------------------------------------------------------------2
-- TODO: Usar plutus-1.1.0
networkId :: Ledger.NetworkId
networkId = CardanoNodeEmulatorParams.testnet

--------------------------------------------------------------------------------2
