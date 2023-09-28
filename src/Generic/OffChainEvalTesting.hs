{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell       #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Generic.OffChainEvalTesting where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Cardano.Api                                     as CardanoApi
import qualified Cardano.Api.Shelley                             as CardanoApiShelley
import qualified Cardano.Binary                                  as CardanoBinary (ToCBOR, serializeEncoding)
import qualified Cardano.Ledger.Alonzo.Scripts                   as AlonzoScripts
import qualified Cardano.Ledger.Alonzo.Tx                        as AlonzoTx
import qualified Cardano.Ledger.Alonzo.TxWitness                 as AlonzoTxWitness
import qualified Cardano.Ledger.Babbage.TxBody                   as BabbageTxBody
import qualified Cardano.Ledger.Babbage.TxInfo                   as BabbageTxInfo
import qualified Cardano.Ledger.Core                             as CardanoLedgerCore
import qualified Cardano.Node.Emulator.TimeSlot                  as CardanoNodeEmulatorTimeSlot
import qualified Cardano.Node.Emulator.Validation                as CardanoNodeEmulatorValidation
import qualified Control.Lens                                    as ControlLens
import qualified Control.Monad as ControlMonad
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.Map                                        as DataMap
import qualified Data.Text                                       as DataText
import qualified Data.Typeable                                   as DataTypeable (Typeable)
import qualified Data.Void                                       as DataVoid (Void)
import qualified Data.SatInt as DataSatInt
import qualified GHC.Natural                                     as GHCNatural
import qualified Ledger
import qualified Ledger.Constraints                              as LedgerConstraints
import qualified Ledger.Tx                                       as LedgerTx
import qualified Ledger.Tx.CardanoAPI                            as LedgerTxCardanoAPI
import qualified Ledger.Tx.CardanoAPI.Internal                   as LedgerTxCardanoAPIInternal
import qualified Ledger.Tx.Constraints                           as LedgerTxConstraints
import qualified Plutus.Contract                                 as PlutusContract
import qualified Plutus.Contract.CardanoAPI                      as PlutusContractCardanoAPI
import qualified Plutus.ChainIndex                               as ChainIndex
import qualified Plutus.Script.Utils.V1.Typed.Scripts.Validators as UtilsTypedScriptsValidatorsV1 (DatumType, RedeemerType)
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import           PlutusTx.Prelude                                hiding (unless)
import qualified PlutusTx.Prelude                                as PlutusTxPrelude
import qualified Prelude                                         as P
import qualified Text.Printf                                     as TextPrintf (printf)

import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath (dropExtension, (</>), takeExtension, takeDirectory, takeFileName)
import Data.Aeson (decode, withObject, (.:))
import Data.ByteString.Lazy (readFile, ByteString)
import Data.Maybe (listToMaybe)

import qualified Data.Default as DataDefault
import qualified Control.Monad as Monad
import qualified GHC.Generics as Generic
import qualified Data.Aeson as DataAeson

import qualified PlutusCore  as PLC
import qualified PlutusCore.Evaluation.Machine.ExBudget  as PLC
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Core.Type as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek  as UPLC

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (..))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusCore.Pretty (prettyPlcClassicDebug)
import PlutusTx (getPlc, compile, applyCode, liftCode, UnsafeFromData (unsafeFromBuiltinData))
import qualified PlutusTx.Code as PlutusTxCode
import qualified Protocol.Fund.Types as FundT
import qualified PlutusTx.Ratio as TxRatio
import qualified Ledger.Ada as LedgerAda
import qualified Ledger 
import qualified PlutusTx.Builtins as TxBuiltins


--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OffChainHelpers                         as OffChainHelpers
import qualified Generic.OnChainHelpers                          as OnChainHelpers
import qualified Protocol.Fund.Helpers as Helpers
import qualified Generic.DeployHelpers as DeployHelpers
import qualified Ledger.Value as LedgerValue
import qualified Generic.OnChainHelpers as Helpers
import qualified PlutusTx.AssocMap as TxAssocMap
import qualified Protocol.InvestUnit.Types as InvestUnitT
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Constants as T
import qualified Plutonomy
import qualified Protocol.Types as T


--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

plcProgramTerm :: PlutusTxCode.CompiledCode a -> UPLC.Term UPLC.NamedDeBruijn DefaultUni  DefaultFun ()
plcProgramTerm = UPLC._progTerm . PlutusTxCode.getPlc

evaluateCompileCodeWithCek :: PlutusTxCode.CompiledCode a -> UPLC.EvaluationResult (UPLC.Term UPLC.NamedDeBruijn DefaultUni DefaultFun ())
evaluateCompileCodeWithCek = UPLC.unsafeExtractEvaluationResult . (\(fstT,_,_) -> fstT) . UPLC.runCekDeBruijn PLC.defaultCekParameters UPLC.restrictingEnormous UPLC.logEmitter . plcProgramTerm

evaluateCompileCodeWithCekGetCost :: PlutusTxCode.CompiledCode a -> (PLC.ExBudget, [DataText.Text])
evaluateCompileCodeWithCekGetCost code  =  
    let (result, UPLC.TallyingSt _ budget, logOut) = UPLC.runCekDeBruijn PLC.defaultCekParameters UPLC.tallying UPLC.logEmitter  (plcProgramTerm code)
    in case result of
            Right _ -> (budget, logOut)
            Left _ -> (budget, logOut)

--------------------------------------------------------------------------------2

{-# INLINEABLE testFunc #-}
testFunc:: Bool
testFunc = 
    a + b == 10  
    where
        a = 4 ::Integer
        b = 6

testFuncCode :: PlutusTxCode.CompiledCode Bool
testFuncCode = $$( PlutusTx.compile [|| testFunc ||])

--------------------------------------------------------------------------------2


calculateDepositComissionsUsingMonthsWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  (Integer, Integer, Integer)
calculateDepositComissionsUsingMonthsWrapper commissionsPerYearInBPx1e3 deadline date deposit  = do
    let 
        !commissionsPerYearInBPx1e3' =  PlutusTx.unsafeFromBuiltinData commissionsPerYearInBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !deposit' =  PlutusTx.unsafeFromBuiltinData deposit
        !res = Helpers.calculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3' deadline' date' deposit'
    res



calculateWithdrawComissionsUsingMonthsWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  (Integer, Integer, Integer)
calculateWithdrawComissionsUsingMonthsWrapper commissionsPerYearInBPx1e3 deadline date withdraw  = do
    let 
        !commissionsPerYearInBPx1e3' =  PlutusTx.unsafeFromBuiltinData commissionsPerYearInBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !withdraw' =  PlutusTx.unsafeFromBuiltinData withdraw
        !res = Helpers.calculateWithdrawComissionsUsingMonths commissionsPerYearInBPx1e3' deadline' date' withdraw'
    res


calculateDepositComissionsUsingMonthsBuiltinDataCodeOptimized :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateDepositComissionsUsingMonthsBuiltinDataCodeOptimized commissionsPerYearInBPx1e3 deadline date deposit = 
    Plutonomy.optimizeUPLC $ calculateDepositComissionsUsingMonthsBuiltinDataCode commissionsPerYearInBPx1e3 deadline date deposit


calculateDepositComissionsUsingMonthsBuiltinDataCode ::Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateDepositComissionsUsingMonthsBuiltinDataCode commissionsPerYearInBPx1e3 deadline date deposit = 
    $$(PlutusTx.compile [|| calculateDepositComissionsUsingMonthsWrapper ||])  
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commissionsPerYearInBPx1e3)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData deposit)

calculateWithdrawComissionsUsingMonthsBuiltinDataCode ::Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateWithdrawComissionsUsingMonthsBuiltinDataCode commissionsPerYearInBPx1e3 deadline date withdraw = 
    $$(PlutusTx.compile [|| calculateWithdrawComissionsUsingMonthsWrapper ||])  
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commissionsPerYearInBPx1e3)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData withdraw)


--------------------------------------------------------------------------------

powRationalWrapper :: BuiltinData -> BuiltinData ->  BuiltinData -> Rational
powRationalWrapper !num !dem !pot  = do
    let 
        !num' =  PlutusTx.unsafeFromBuiltinData num
        !dem' =  PlutusTx.unsafeFromBuiltinData dem
        !pot' =  PlutusTx.unsafeFromBuiltinData pot
        !res = Helpers.powRational num' dem' pot'
    res


powRationalWrapperBuiltinDataCodeOptimized :: Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
powRationalWrapperBuiltinDataCodeOptimized num dem pot = 
    Plutonomy.optimizeUPLC $ powRationalWrapperBuiltinDataCode num dem pot

powRationalWrapperBuiltinDataCode :: Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
powRationalWrapperBuiltinDataCode num dem pot = 
    Plutonomy.optimizeUPLC $    $$(PlutusTx.compile [|| powRationalWrapper ||])  
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData num)
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData dem)
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData pot)
--------------------------------------------------------------------------------


-- ESTO ES LO QUE QUIERO OPTIMIZAR

testDepositWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData ->  Bool
testDepositWrapper commissionsPerYearInBPx1e3 deadline date deposit  fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit  = do
    let 
        !commissionsPerYearInBPx1e3' =  PlutusTx.unsafeFromBuiltinData commissionsPerYearInBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !deposit' =  PlutusTx.unsafeFromBuiltinData deposit
        !fundFT_AC' =  PlutusTx.unsafeFromBuiltinData fundFT_AC
        !valueOf_FundHoldingDatum_In' =  PlutusTx.unsafeFromBuiltinData valueOf_FundHoldingDatum_In
        !valueOf_FundHoldingDatum_Out' =  PlutusTx.unsafeFromBuiltinData valueOf_FundHoldingDatum_Out
        !investUnit' =  PlutusTx.unsafeFromBuiltinData investUnit
        !res = testDeposit commissionsPerYearInBPx1e3' deadline' date' deposit' fundFT_AC' valueOf_FundHoldingDatum_In' valueOf_FundHoldingDatum_Out' investUnit'
    res

-- valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit 
-- -> LedgerValue.Value -> LedgerValue.Value -> T.InvestUnit -> 

-- instance FromBuiltin LedgerApiV2.AssetClass Bool where
--     {-# INLINABLE fromBuiltin #-}
--     fromBuiltin b = ifThenElse b True False
-- instance ToBuiltin Bool BuiltinBool where
--     {-# INLINABLE toBuiltin #-}
--     toBuiltin b = if b then true else false

-- currencySymbolToBuiltinData :: LedgerApiV2.CurrencySymbol -> BuiltinData
-- currencySymbolToBuiltinData = TxBuiltins.mkB . LedgerApiV2.unCurrencySymbol

-- tokenNameToBuiltinData :: LedgerApiV2.TokenName -> BuiltinData
-- tokenNameToBuiltinData = TxBuiltins.mkB . LedgerApiV2.unTokenName

-- assetClassToBuiltinData :: LedgerValue.AssetClass -> BuiltinData
-- assetClassToBuiltinData ac = 
--     let 
--         (cs, tn) = LedgerValue.unAssetClass ac
--         !cs' = currencySymbolToBuiltinData cs
--         !tn' = tokenNameToBuiltinData tn
--     in TxBuiltins.mkList  [cs', tn' ]

-- valueToBuiltinData :: LedgerValue.Value -> BuiltinData
-- valueToBuiltinData v  = 
--     LedgerApiV2.toBuiltinData  v
    

testDepositBuiltinDataCodeOptimized :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value ->  T.InvestUnit -> PlutusTxCode.CompiledCode Bool
testDepositBuiltinDataCodeOptimized commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit = 
    Plutonomy.optimizeUPLC $ testDepositBuiltinDataCode commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit

testDepositBuiltinDataCode :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value ->  T.InvestUnit -> PlutusTxCode.CompiledCode Bool
testDepositBuiltinDataCode commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit = 
           $$(PlutusTx.compile [|| testDepositWrapper ||])  
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commissionsPerYearInBPx1e3)
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
                    -- `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData deposit)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData commissionsPerYearInBPx1e3)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData deadline)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData date)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData deposit)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData fundFT_AC)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData valueOf_FundHoldingDatum_In)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData valueOf_FundHoldingDatum_Out)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData investUnit)


fundFT_AC_ :: LedgerValue.AssetClass
fundFT_AC_ = LedgerValue.AssetClass ("d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb",  T.fundFT_TN)

valueOf_FundHoldingDatum_In_ :: LedgerValue.Value
valueOf_FundHoldingDatum_In_ = LedgerAda.lovelaceValueOf 12241330 <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1

-- (,\"\",12241330)"
-- (6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759,\"token1\",1000000)
-- (74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa,\"FundHoldingID0\",1)
-- (d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb,\"FT\",24703)


valueOf_FundHoldingDatum_Out_ :: LedgerValue.Value
valueOf_FundHoldingDatum_Out_ = LedgerAda.lovelaceValueOf 12241330  <> LedgerApiV2.singleton "74854c7cd622e151aeef59b7d97fe0d60e8e69a10adbe13c19e918aa" "FundHoldingID0" 1 <>  LedgerApiV2.singleton "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759" "token" 1000000 <>  LedgerApiV2.singleton "d158327544bca825cdabd07f96727f64f5262fcfbf661c3b5f9118cb"  T.fundFT_TN 12923

token1_ :: T.InvestUnitToken
token1_ = ( "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759","token", 1) :: T.InvestUnitToken

investUnit_ :: T.InvestUnit
investUnit_ = T.InvestUnit { T.iuValues = [] }

--  
-- 

testDeposit :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer ->  LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value -> T.InvestUnit ->   Bool
testDeposit commissionsPerYearInBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
    --traceIfFalse "not isCorrect_Output_FundHoldingDatum_Updated_With_Deposit" (isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control_With_Deposit)
    -- && 
    traceIfFalse "not isCorrect_Output_FundHoldingDatum_Value_WithTokensAndFT" (isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control_WithTokensAndFT valueOf_FundHoldingDatum_Out)
    --     && 
    --     traceIfFalse "not isMintingFT" isMintingFT
    --     && traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info)
    where
    ------------------
        !(userFT, commissionsFT, commissions_RatePerMonth_Numerator1e6) = Helpers.calculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date deposit 
        -- !(userFT, commissionsFT, commissions_RatePerMonth_Numerator1e6) = (987077,12923,1076916666)
    ------------------
        -- !valueOf_TokensForDeposit = createValue_WithTokensFrom_InvestUnit deposit
        !valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value = createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value valueOf_FundHoldingDatum_In deposit  investUnit
    ------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
    ------------------
        !valueFor_FundHoldingDatum_Control_WithTokensAndFT = valueOf_TokensForDeposit_Plus_FundHoldingDatum_Value <> valueFor_FT_Commissions -- valueOf_FundHoldingDatum_In -- <> valueOf_TokensForDeposit <> valueFor_FT_Commissions
    ------------------
        -- !fundHoldingDatum_Control_With_Deposit = Helpers.mkUpdated_FundHoldingDatum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_RatePerMonth_Numerator1e6
    ------------------ 
        -- isMintingFT :: Bool
        -- isMintingFT = Helpers.isToken_Minting_With_AC_AndAmt fundFT_AC deposit info

-- isCorrect_Output_FundHoldingDatum :: FundHoldingT.FundHoldingDatumType -> Bool
-- isCorrect_Output_FundHoldingDatum fundHoldingDatum_Control =
--     let !fundHoldingDatum_Out = Helpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
--     in  fundHoldingDatum_Out `Helpers.isUnsafeEqDatums` fundHoldingDatum_Control
------------------

isCorrect_Output_FundHoldingDatum_Value :: LedgerValue.Value -> LedgerValue.Value -> Bool
isCorrect_Output_FundHoldingDatum_Value valueFor_FundHoldingDatum_Control valueOf_FundHoldingDatum_Out =
    valueOf_FundHoldingDatum_Out `Helpers.isEqValue` valueFor_FundHoldingDatum_Control

------------------

createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value :: LedgerValue.Value -> Integer -> T.InvestUnit -> LedgerValue.Value
createValue_WithTokensFrom_InvestUnit_Plus_FundHoldingDatum_Value (LedgerValue.Value mp) amount investUnit = 
    LedgerValue.Value mapCSResult
    where
        !listMapCS = TxAssocMap.toList mp
        !investUnitTokens = T.iuValues investUnit
        !listTokens =  investUnitTokens --TODO: sort solo por CS
        !mapCSResult = TxAssocMap.fromList (updateListMapCS listTokens listMapCS)
    ------------------
        updateListMapCS [] restListMapCS = restListMapCS
        updateListMapCS ((cs, tn, amt): restTokens) restListMapCS  =
            let 
                !(tokensFromSameCS, restTokensWithoutCS) = getOthersTokensFromSameCSAndDeleteFromList cs restTokens [] []
                !(mapFromSameCS, restMapWithoutCS) = getMapFromSameCSAndDeleteFromList cs restListMapCS []
                !mapFromSameCSWithTokensAdded = addTokensInMap cs mapFromSameCS ((tn, amt):tokensFromSameCS)
                !resultMap = mapFromSameCSWithTokensAdded : updateListMapCS restTokensWithoutCS restMapWithoutCS 
            in resultMap -- updateListMapCS restListCS remainIU (accListCS ++ [(cs, mapTNResult)])
    ------------------
        getOthersTokensFromSameCSAndDeleteFromList _ [] accListFromSame accListOthers = (accListFromSame, accListOthers)
        getOthersTokensFromSameCSAndDeleteFromList cs ((cs', tn', amt'): restTokens) accListFromSame accListOthers
            | cs == cs' = getOthersTokensFromSameCSAndDeleteFromList cs restTokens ( (tn', amt'): accListFromSame) accListOthers
            | otherwise = getOthersTokensFromSameCSAndDeleteFromList cs restTokens accListFromSame ( (cs', tn', amt'): accListOthers)
    ------------------
        getMapFromSameCSAndDeleteFromList _ [] accListMapOthers = (Nothing, accListMapOthers)
        getMapFromSameCSAndDeleteFromList cs ((cs', mapTN): restMap) accListOthers
            | cs == cs' = (Just mapTN, restMap ++ accListOthers)
            | otherwise = getMapFromSameCSAndDeleteFromList cs restMap ( (cs', mapTN): accListOthers)
    ------------------
        addTokensInMap cs Nothing tokens = addTokensInMap' cs TxAssocMap.empty tokens -- createMapFromTokens cs tokens 
        addTokensInMap cs (Just mapTN) tokens = addTokensInMap' cs mapTN tokens
    ------------------
        addTokensInMap' cs mapTN [] = (cs, mapTN)
        addTokensInMap' cs mapTN listTokensToAddInMap = (cs, foldl (\acc (tn', amt') -> mapElement acc tn' (amt' * amount)) mapTN listTokensToAddInMap)
    ------------------
        mapElement acc tn amt = 
            case TxAssocMap.lookup tn acc of
                Nothing -> TxAssocMap.insert tn amt acc
                Just amt' -> TxAssocMap.insert tn (amt + amt') acc
    ------------------
        -- !listCS = TxAssocMap.toList mp
        -- !mapCSResult = TxAssocMap.fromList (updateListCS listCS investUnitTokens [])

        -- updateListCS restListCS              []     accListCS = restListCS ++ accListCS
        -- updateListCS []                      listIU      accListCS = accListCS 
        -- updateListCS ((cs,mapTN):restListCS) listIU accListCS =
        --     let
        --         !listNT = TxAssocMap.toList mapTN
        --         !(listTNResult, remainIU) = updateListTN cs listNT [] listIU  []  
        --         !mapTNResult = TxAssocMap.fromList listTNResult
        --     in updateListCS restListCS remainIU (accListCS ++ [(cs, mapTNResult)])
    
        -- updateListTN _  []                    revisedTnAmt  listIU                       revisedIU = (revisedTnAmt, listIU ++ revisedIU) 
        -- updateListTN cs ((tn, amt):restTnAmt) revisedTnAmt  []                           revisedIU = updateListTN cs restTnAmt                           ((tn, amt): revisedTnAmt) revisedIU  [] 
        -- updateListTN cs ((tn, amt):restTnAmt) revisedTnAmt ((cs', tn', amt'):restListUI) revisedIU 
        --     | cs == cs' && tn == tn'                                                               = updateListTN cs ((tn, amt+ amt' * amount):restTnAmt) revisedTnAmt              restListUI revisedIU 
        --     | not (cs == cs' && tn == tn')                                                         = updateListTN cs ((tn, amt)              :restTnAmt) revisedTnAmt              restListUI ((cs', tn', amt'):revisedIU)
        -- updateListTN _  _                     _             _                            _         = ([], []) 

        
        -- updateListCS' (mapCS:restMapsCS) !resultMapCSAux !remainIU = 
        --         let (newMapCS, remainIUAux) = finishWithThisMapCS mapCS remainIU 
        --         in updateListCS' restMapsCS (resultMapCSAux ++ [newMapCS]) remainIUAux
        -- finishWithThisMapCS (!cs, !tnMap) [] =  ((cs, tnMap) , remainIUAux)
        -- finishWithThisMapCS (!cs, !tnMap) !remainIU 
        --     -- quiero que busque todo lo que hay en invest unit de esta cs
        --     -- y la agrege al mapListde TN
        --     -- que devuelva un map actualizado de esta CS con los TN y amt actualizados y el resto de la lista de invest unit 
        --         let 
        --             (listTN, !remainIUAux) = finishWithThisMapTN (!cs, !tnMap) remainIU []
        --         in ((cs, listTN), remainIUAux)
                    
        -- finishWithThisMapTN (!cs, !tnMap) [] =  ((cs, tnMap) , remainIUAux)
        -- finishWithThisMapTN (!cs, !tnMap) !remainIU   
            -- quiero que busque todo lo que hay en invest unit de esta cs y tn      