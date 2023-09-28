{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds #-}

module Main where

------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------

import qualified Control.Monad          as ControlMonad (mapM, replicateM, unless)
import qualified GHC.Generics           as Generics
import qualified Ledger
import qualified Ledger.Address         as LedgerAddress (PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey (PaymentPubKey))
import qualified Ledger.Crypto          as Crypto
import qualified Plutus.Model           as PlutusSimpleModel (Ada (Lovelace), DatumMode (HashDatum), Run, Tx, TypedValidator (TypedValidator), UserSpend, ada, adaValue, currentTimeRad, defaultBabbage, logError, mustFail, newUser, payToKey,
                                                              payToScript, spend, spendScript, submitTx, testNoErrors, toV2, userSpend, utxoAt, validateIn, valueAt, waitUntil)
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import qualified PlutusTx               (FromData (fromBuiltinData), ToData (toBuiltinData), compile, makeIsDataIndexed, makeLift, unstableMakeIsData)
import           PlutusTx.Builtins      (BuiltinData, Integer, mkI)
import           PlutusTx.Prelude       hiding (unless)
import qualified Prelude                as P
import qualified Test.Tasty             as Tasty

------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.OffChainHelpers as OffChainHelpers

------------------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------------------

-- SIMPLE VALIDATOR

data CustomDatum
    = MkCustomDatum
          { signedMessage :: LedgerApiV2.BuiltinByteString
          , signature     :: Crypto.Signature
          }
    deriving (Generics.Generic, P.Show)

PlutusTx.unstableMakeIsData ''CustomDatum

newtype CustomRedeemer
    = MkCustomRedeemer { publickey :: LedgerAddress.PaymentPubKey }
    deriving (Generics.Generic, P.Show)

PlutusTx.unstableMakeIsData ''CustomRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatum -> CustomRedeemer -> LedgerApiV2.ScriptContext -> Bool
mkValidator (MkCustomDatum signedMsg signature) (MkCustomRedeemer pkh) ctx =
    traceIfFalse "wrong public key" chekSignatureOfMsgWithPkh
    where
        chekSignatureOfMsgWithPkh :: Bool
        chekSignatureOfMsgWithPkh = case OnChainHelpers.checkSignature pkh signedMsg signature of
            Left _  -> False
            Right _ -> True

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: LedgerApiV2.Validator
validator = LedgerApiV2.mkValidatorScript $$(PlutusTx.compile [|| mkWrappedValidator ||])

{-# INLINABLE wrapValidator #-}
wrapValidator :: ( LedgerApiV2.UnsafeFromData datum , LedgerApiV2.UnsafeFromData redeemer )
              => (datum -> redeemer -> LedgerApiV2.ScriptContext -> Bool)
              -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator validator' datum redeemer ctx =
  check $ validator'
      (LedgerApiV2.unsafeFromBuiltinData datum)
      (LedgerApiV2.unsafeFromBuiltinData redeemer)
      (LedgerApiV2.unsafeFromBuiltinData ctx)

------------------------------------------------------------------------------------------

-- Validator's script
valScript :: PlutusSimpleModel.TypedValidator datum redeemer
valScript = PlutusSimpleModel.TypedValidator $ PlutusSimpleModel.toV2 validator

-- Set many users at once
setupUsers :: PlutusSimpleModel.Run [LedgerApiV2.PubKeyHash]
setupUsers = ControlMonad.replicateM 2 $ PlutusSimpleModel.newUser $ PlutusSimpleModel.ada (PlutusSimpleModel.Lovelace 1000)

-- Create transaction that spends "spUTXO" to lock "val" in "valScript"
lockingTx :: PlutusSimpleModel.UserSpend -> CustomDatum -> LedgerApiV2.Value -> PlutusSimpleModel.Tx
lockingTx  spUTXO datum val =
    P.mconcat
      [ PlutusSimpleModel.userSpend spUTXO
      , PlutusSimpleModel.payToScript valScript (PlutusSimpleModel.HashDatum datum) val
      ]

-- Create transaction that spends "refUTXO" to unlock "val" from the "valScript" validator
consumingTx :: CustomDatum -> CustomRedeemer -> LedgerApiV2.PubKeyHash -> LedgerApiV2.TxOutRef -> LedgerApiV2.Value -> PlutusSimpleModel.Tx
consumingTx  datum redeemer usrPKH refUTXO val =
    P.mconcat
      [ PlutusSimpleModel.spendScript valScript refUTXO redeemer datum
      , PlutusSimpleModel.payToKey usrPKH val
      ]

waitBeforeConsumingTx :: LedgerApiV2.POSIXTime
waitBeforeConsumingTx = 1000

------------------------------------------------------------------------------------------

-- Function to test if both creating and consuming script UTxOs works properly
testScript :: CustomDatum -> CustomRedeemer -> PlutusSimpleModel.Run ()
testScript datum redeemer = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 Lovelaces ("val") IN VALIDATOR
  let val = PlutusSimpleModel.adaValue 100                    -- Define value to be transfered
  spUTXO <- PlutusSimpleModel.spend u1 val                    -- Get user's UTXO that we should spend
  PlutusSimpleModel.submitTx u1 $ lockingTx spUTXO datum val  -- User 1 submits "lockingTx" transaction
  -- WAIT FOR A BIT
  PlutusSimpleModel.waitUntil waitBeforeConsumingTx

  -- USER 2 TAKES "val" FROM VALIDATOR
  utxos <- PlutusSimpleModel.utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(ref, out)] = utxos                  -- We know there is only one UTXO (the one we created before)
  ct <- PlutusSimpleModel.currentTimeRad 100                  -- Create time interval with equal radius around current time
  tx <- PlutusSimpleModel.validateIn ct $ consumingTx datum redeemer u2 ref (LedgerApiV2.txOutValue out)  -- Build Tx
  PlutusSimpleModel.submitTx u2 tx                            -- User 2 submits "consumingTx" transaction
  -- CHECK THAT FINAL BALANCES MATCH EXPECTED BALANCES
  [v1, v2] <- ControlMonad.mapM PlutusSimpleModel.valueAt [u1, u2]       -- Get final balances of both users
  ControlMonad.unless (v1 == PlutusSimpleModel.adaValue 900 && v2 == PlutusSimpleModel.adaValue 1100) $  -- Check if final balances match expected balances
    PlutusSimpleModel.logError "Final balances are incorrect"

------------------------------------------------------------------------------------------

main :: P.IO ()
main =
  let
    message = OffChainHelpers.stringToBuiltinByteString "" :: BuiltinByteString
    -----------------
    oraclePrivateKey =  Crypto.generateFromSeed' $ OffChainHelpers.stringToStrictByteString "dad cupboard hotel cause mansion feature oppose prevent install venue finish galaxy tone green volcano neglect coil toast future exchange prize social next tape"
    oraclePublicKey = Crypto.toPublicKey oraclePrivateKey
    oraclePPK = Ledger.PaymentPubKey oraclePublicKey
    oracleSignature   = Crypto.sign' message oraclePrivateKey
    -----------------
    oracleFalsePrivateKey =  Crypto.generateFromSeed' "he eeeee he ds fd gg ge eew rer trt erw rwerwe trter gfgdf gfdgdf rtet trtre treter ghfhgf treter gfdgdf tretre gfdgdf tretre"
    oracleFalsePublicKey = Crypto.toPublicKey oracleFalsePrivateKey
    oracleFalsePPK = Ledger.PaymentPubKey oracleFalsePublicKey
    -----------------
    datum = MkCustomDatum message oracleSignature
    redeemerOK = MkCustomRedeemer oraclePPK
    redeemerBad = MkCustomRedeemer oracleFalsePPK
    -----------------
  in
    do
        Tasty.defaultMain $ do
            Tasty.testGroup
                "Testing validator with some sensible values"
                [ good "User 1 locks and user 2 takes with Good Oracle PK - succeeds" $ testScript datum redeemerOK
                , bad  "User 1 locks and user 2 takes with Bad Oracle PK - fails  " $ testScript datum redeemerBad
                ]
            where
            bad msg = good msg . PlutusSimpleModel.mustFail
            good = PlutusSimpleModel.testNoErrors (PlutusSimpleModel.adaValue 10_000_000) PlutusSimpleModel.defaultBabbage

------------------------------------------------------------------------------------------
