{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.SellOffer.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad                       as Monad
import qualified Data.Map                            as DataMap
import qualified Data.Text                           as DataText (Text)
import qualified Data.Void                           as DataVoid (Void)
import qualified Ledger                              (PaymentPubKeyHash (..), pubKeyHashAddress, unPaymentPubKeyHash, unStakePubKeyHash)
import qualified Ledger.Ada                          as LedgerAda
import qualified Ledger.Constraints                  as LedgerConstraints
import qualified Ledger.Constraints.OffChain         as LedgerConstraintsOffChain
import qualified Ledger.Constraints.TxConstraints    as LedgerTxConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Tx                           as LedgerTx
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
import qualified Generic.OffChainEval                as OffChainEval
import qualified Generic.OffChainHelpers             as OffChainHelpers
import qualified Generic.OnChainHelpers              as OnChainHelpers
import qualified Protocol.Constants                  as Constants
import qualified Protocol.Protocol.Types                 as ProtocolT
import qualified Protocol.PABTypes                   as T
import qualified Protocol.SellOffer.Types              as T
import qualified Protocol.Types                      as T
import qualified Protocol.OffChainHelpers as OffChainHelpers

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2
