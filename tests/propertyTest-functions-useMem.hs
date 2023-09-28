{-# LANGUAGE DataKinds           #-}
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

------------------------------------------------------------------------------------------
-- External Imports
------------------------------------------------------------------------------------------

import qualified Control.Monad               as ControlMonad (mapM, replicateM, unless)
import qualified GHC.Generics                as Generics
import qualified Ledger
import qualified Ledger.Address              as LedgerAddress (PaymentPrivateKey (unPaymentPrivateKey), PaymentPubKey (PaymentPubKey))
import qualified Ledger.Crypto               as Crypto
import qualified Plutus.Model                as PlutusSimpleModel (Ada (Lovelace), DatumMode (HashDatum), Run, Tx, TypedValidator (TypedValidator), UserSpend, ada, adaValue, currentTimeRad, defaultBabbage, logError, mustFail, newUser, payToKey,
                                                                                                                                     payToScript, spend, spendScript, submitTx, testNoErrors, toV2, userSpend, utxoAt, validateIn, valueAt, waitUntil)
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified PlutusTx                    (FromData (fromBuiltinData), ToData (toBuiltinData), compile, makeIsDataIndexed, makeLift, unstableMakeIsData)
import           PlutusTx.Builtins           (BuiltinData, Integer, mkI)
import           PlutusTx.Prelude            hiding (unless)
import qualified Prelude                     as P

import qualified Data.SatInt                 as DataSatInt
import           Test.QuickCheck             as QC
import           Test.QuickCheck.Monadic     as QCMondadic
import qualified Test.Tasty                  as Tasty
import qualified Test.Tasty.QuickCheck       as TastyQC
import qualified System.Random as Random(randomRIO)

------------------------------------------------------------------------------------------
-- Internal Imports
------------------------------------------------------------------------------------------
import qualified Generic.OffChainEvalTesting as OffChainEvalTesting
import qualified Generic.OnChainHelpers      as OnChainHelpers
import qualified Ledger.Ada                  as LedgerAda
import qualified Ledger.Value                as LedgerValue
import qualified Protocol.Constants          as T
import qualified Protocol.Fund.Helpers       as Helpers
import qualified Protocol.InvestUnit.Types   as InvestUnitT
import qualified Generic.OffChainHelpers as OffChainHelpers

------------------------------------------------------------------------------------------
-- Module
------------------------------------------------------------------------------------------

-- instance Arbitrary LedgerApiV2.POSIXTime where
--   arbitrary = do
--     n <- choose (1691965154000, 1755123554000)
--     return (LedgerApiV2.POSIXTime n)

-- | Property testing
main ::P.IO ()
main = Tasty.defaultMain $ do
    Tasty.testGroup
        "Testing resources use"
        [   
            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 500) $
            -- TastyQC.testProperty "PowRational should use less than 3.5Mb" (propPowRational_less_than False 3_500_000 ),
            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 500) $
            -- TastyQC.testProperty "CalculateDepositComissionsUsingMonths should use less than 6.5Mb" (propCalculateDepositComissionsUsingMonths_less_than False 6_500_000 ),
            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 500) $
            -- TastyQC.testProperty "Deposits should use less than 10Mb" (propDeposit_less_than False 10_000_000 ),
            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 500) $
            -- TastyQC.testProperty "Deposits should be valid" (propDeposit_isValid False),

            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 1000) $
            -- TastyQC.testProperty "PowRational Optimized should use less than 1.4Mb" (propPowRational_less_than True 1_400_000 ),
            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 1000) $
            -- TastyQC.testProperty "CalculateDepositComissionsUsingMonths Optimized should use less than 2.5Mb" (propCalculateDepositComissionsUsingMonths_less_than True 2_500_000 ),
            Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 10) $
            TastyQC.testProperty "Deposits Optimized with 10 max tokens should use less than 6.2Mb" (propDeposit_less_than True 6_250_000 )
            -- Tasty.adjustOption (\_ -> TastyQC.QuickCheckTests 500) $
            -- TastyQC.testProperty "Deposits Optimized should be valid" ( propDeposit_isValid True)
        ]

---------------------------------------------------------------

data TestParamsCalculateDepositComissionsUsingMonths
    = TestParamsCalculateDepositComissionsUsingMonths Integer LedgerApiV2.POSIXTime LedgerApiV2.POSIXTime Integer
    deriving (P.Eq, P.Show)

instance Arbitrary TestParamsCalculateDepositComissionsUsingMonths where
    arbitrary = do
        commissionsPerYearInBPx1e3 <- choose (1, 10000000)
        deadline' <- choose (1691965154000, 1755123554000)
        date' <- choose (1691965154000, 1755123554000)
        deposit <- choose (1000000, 10000000000)
        return (TestParamsCalculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 (LedgerApiV2.POSIXTime deadline') (LedgerApiV2.POSIXTime date') deposit)


propCalculateDepositComissionsUsingMonths_less_than :: Bool -> Integer ->  TestParamsCalculateDepositComissionsUsingMonths -> TastyQC.Property
propCalculateDepositComissionsUsingMonths_less_than useOptimized memMax (TestParamsCalculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date deposit) =
    (deadline > date) QC.==>
        runCheckCalculateDepositComissionsUsingMonths_less_than useOptimized memMax commissionsPerYearInBPx1e3 deadline date deposit

runCheckCalculateDepositComissionsUsingMonths_less_than :: Bool -> Integer -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Property
runCheckCalculateDepositComissionsUsingMonths_less_than useOptimized memMax commissionsPerYearInBPx1e3 deadline date deposit =
    let evalCostResult = 
            if useOptimized then 
                OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.calculateDepositComissionsUsingMonthsBuiltinDataCodeOptimized commissionsPerYearInBPx1e3 deadline date deposit)
            else 
                OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.calculateDepositComissionsUsingMonthsBuiltinDataCode commissionsPerYearInBPx1e3 deadline date deposit)
        (LedgerApiV2.ExBudget _ (LedgerApiV2.ExMemory  mem), _) = evalCostResult
        memInt = P.read @Integer (P.show mem)
    in counterexample (P.show evalCostResult) (memInt < memMax)

---------------------------------------------------------------

data TestParamsPowRational
    = TestParamsPowRational Integer Integer Integer
    deriving (P.Eq, P.Show)

instance Arbitrary TestParamsPowRational where
    arbitrary = do
        num <- choose (0, 120_000_000)
        den <- choose (0, 120_000_000)
        n <- choose (0, 25)
        return (TestParamsPowRational num den n)

propPowRational_less_than :: Bool -> Integer -> TestParamsPowRational -> TastyQC.Property
propPowRational_less_than useOptimized memMax  (TestParamsPowRational num den n) =
    True QC.==>
            runCheckPowRational_less_than useOptimized memMax num den n


runCheckPowRational_less_than :: Bool -> Integer -> Integer -> Integer -> Integer -> Property
runCheckPowRational_less_than useOptimized memMax num den n =
    let evalCostResult = 
            if useOptimized then 
                OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.powRationalWrapperBuiltinDataCodeOptimized num den n) 
            else 
                OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.powRationalWrapperBuiltinDataCode num den n)

        (LedgerApiV2.ExBudget _ (LedgerApiV2.ExMemory  mem), _) = evalCostResult
        memInt = P.read @Integer (P.show mem)
    in counterexample (P.show evalCostResult) (memInt < memMax)


------------------------------------------------------------------------------------------

data TestParamsDeposit
    = TestParamsDeposit [InvestUnitT.InvestUnitToken] Integer LedgerApiV2.POSIXTime LedgerApiV2.POSIXTime Integer
    deriving (P.Eq, P.Show)

instance Arbitrary TestParamsDeposit where
    arbitrary = do
        tokensQty <- choose (5, 10)
        tokens <- ControlMonad.replicateM tokensQty generateRandomToken
        commissionsPerYearInBPx1e3 <- choose (1, 10000000)
        deadline' <- choose (1691965154000, 1755123554000)
        date' <- choose (1691965154000, 1755123554000)
        deposit <- choose (1000000, 10000000000)
        return (TestParamsDeposit tokens commissionsPerYearInBPx1e3 (LedgerApiV2.POSIXTime deadline') (LedgerApiV2.POSIXTime date') deposit)

generateRandomToken :: Gen InvestUnitT.InvestUnitToken
generateRandomToken = do
    randomHex <- ControlMonad.replicateM 64 (elements ['0'..'f'])
    randomTokenName <- ControlMonad.replicateM 5 (elements ['a'..'z'])
    return (LedgerValue.CurrencySymbol $ OffChainHelpers.stringToBuiltinByteString randomHex, LedgerValue.TokenName $ OffChainHelpers.stringToBuiltinByteString randomTokenName, 1)


propDeposit_less_than :: Bool -> Integer -> TestParamsDeposit -> TastyQC.Property
propDeposit_less_than useOptimized memMax (TestParamsDeposit tokens commissionsPerYearInBPx1e3 deadline date deposit) =
    (deadline > date) QC.==>
        runCheckDeposit_less_than useOptimized memMax tokens commissionsPerYearInBPx1e3 deadline date deposit

runCheckDeposit_less_than :: Bool -> Integer -> [InvestUnitT.InvestUnitToken] -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Property
runCheckDeposit_less_than useOptimized memMax tokens commissionsPerYearInBPx1e3 deadline date deposit =
    let
        (fundFT_AC, valueOf_FundHoldingDatum_In, valueOf_FundHoldingDatum_Out, investUnit) = generateDepositParams tokens commissionsPerYearInBPx1e3 deadline date deposit
        evalCostResult = 
            if useOptimized then 
                OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.testDepositBuiltinDataCodeOptimized commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit)
            else 
                OffChainEvalTesting.evaluateCompileCodeWithCekGetCost (OffChainEvalTesting.testDepositBuiltinDataCode commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit)
        (LedgerApiV2.ExBudget _ (LedgerApiV2.ExMemory  mem), _) = evalCostResult
        memInt = P.read @Integer (P.show mem)
    in counterexample (P.show evalCostResult) (memInt < memMax)


propDeposit_isValid :: Bool -> TestParamsDeposit -> TastyQC.Property
propDeposit_isValid useOptimized (TestParamsDeposit tokens commissionsPerYearInBPx1e3 deadline date deposit) =
    (deadline > date) QC.==>
            runCheckDeposit_isValid useOptimized tokens commissionsPerYearInBPx1e3 deadline date deposit

runCheckDeposit_isValid :: Bool -> [InvestUnitT.InvestUnitToken] -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Property
runCheckDeposit_isValid useOptimized tokens commissionsPerYearInBPx1e3 deadline date deposit =
    let
        (fundFT_AC, valueOf_FundHoldingDatum_In, valueOf_FundHoldingDatum_Out, investUnit) = generateDepositParams tokens commissionsPerYearInBPx1e3 deadline date deposit
        isValidDeposit = OffChainEvalTesting.testDeposit commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit
    in counterexample (P.show isValidDeposit) isValidDeposit


generateDepositParams :: [InvestUnitT.InvestUnitToken] -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> (LedgerValue.AssetClass, LedgerValue.Value , LedgerValue.Value , InvestUnitT.InvestUnit)
generateDepositParams tokens commissionsPerYearInBPx1e3 deadline date deposit =
    let
        !(userFT, commissionsFT, commissions_RatePerMonth_Numerator1e6) = Helpers.calculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date deposit

        fundFT_AC :: LedgerValue.AssetClass
        fundFT_AC = LedgerValue.AssetClass ("d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb",  T.fundFT_TN)

        valueOf_FundHoldingDatum_In :: LedgerValue.Value
        valueOf_FundHoldingDatum_In = LedgerAda.lovelaceValueOf 12241330
                    <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1

        -- valueFor_FundHoldingDatum_Out :: LedgerValue.Value
        -- valueFor_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf 12241330
        --             <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1
        --             <>  LedgerApiV2.singleton "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759" "token" deposit
        --             <>  LedgerApiV2.singleton "d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb"  T.fundFT_TN commissionsFT

        investUnit :: InvestUnitT.InvestUnit
        investUnit = InvestUnitT.InvestUnit { InvestUnitT.iuValues = tokens }

        !valueOf_TokensForDeposit = foldl (P.<>) (LedgerAda.lovelaceValueOf 0)
                [LedgerValue.assetClassValue
                    (LedgerValue.AssetClass
                        (cs, tn))
                    (amt * deposit) |
                    (cs, tn, amt) <- tokens]

        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT

        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> valueOf_TokensForDeposit P.<> valueFor_FT_Commissions

        -- token1 :: InvestUnitT.InvestUnitToken
        -- token1 = ( "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759","token", 1) :: InvestUnitT.InvestUnitToken


    in
        (fundFT_AC, valueOf_FundHoldingDatum_In, valueFor_FundHoldingDatum_Out, investUnit)
