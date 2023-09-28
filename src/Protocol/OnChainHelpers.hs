{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}

--------------------------------------------------------------------------------2

module Protocol.OnChainHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad                       as Monad
import qualified Data.Aeson                          as DataAeson
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid
import qualified Ledger
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.TxConstraints    as LedgerTxConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Tx                           as LedgerTx
import qualified Ledger.Typed.Scripts                as UtilsTypedScriptsValidatorsV1
import qualified Ledger.Value                        as LedgerValue
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Text.Printf                         as TextPrintf (printf)

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.Constants                   as T
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Generic.Types                       as T
import qualified Protocol.Constants                  as T
import qualified Protocol.Fund.Helpers               as Helpers
import qualified Protocol.Fund.Holding.Types         as FundHoldingT
import qualified Protocol.Fund.Types                 as FundT
import qualified Protocol.PABTypes                   as T
import qualified Protocol.Protocol.Types             as ProtocolT
import qualified Protocol.Script.Types               as ScriptT
import qualified Protocol.Types                      as T

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

{-# INLINEABLE oracleReIdxDataToBBS #-}
oracleReIdxDataToBBS :: T.OracleReIdx_Data -> BuiltinByteString
oracleReIdxDataToBBS T.OracleReIdx_Data {..} =
    let
        valueToBBS :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> BuiltinByteString
        valueToBBS (cs, tn, amt) = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> OnChainHelpers.intToBBS amt

        investUnitToBBS :: T.InvestUnit -> BuiltinByteString
        investUnitToBBS investUnit  = foldl (<>) emptyByteString (valueToBBS <$> T.iuValues investUnit)

        oridTokensPriceADABBS = investUnitToBBS oridTokensPriceADA
        oridTimeBBS = OnChainHelpers.pOSIXTimeToBBS oridTime

    in  oridTokensPriceADABBS <> oridTimeBBS


{-# INLINEABLE oracleDataToBBS #-}
oracleDataToBBS :: T.Oracle_Data -> BuiltinByteString
oracleDataToBBS T.Oracle_Data {..} =
    let
        valueToBBS :: (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer) -> BuiltinByteString
        valueToBBS (cs, tn, amt) = LedgerApiV2.unCurrencySymbol cs <> LedgerApiV2.unTokenName tn <> OnChainHelpers.intToBBS amt

        investUnitToBBS :: T.InvestUnit -> BuiltinByteString
        investUnitToBBS investUnit  = foldl (<>) emptyByteString (valueToBBS <$> T.iuValues investUnit)

        oridTokensPriceADABBS = investUnitToBBS odFTPriceADA
        oridTimeBBS = OnChainHelpers.pOSIXTimeToBBS odTime

    in  oridTokensPriceADABBS <> oridTimeBBS


--------------------------------------------------------------------------------2

