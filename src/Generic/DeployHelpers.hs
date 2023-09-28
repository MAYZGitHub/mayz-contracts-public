{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Generic.DeployHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Cardano.Api             as CardanoApi
import qualified Cardano.Api.Shelley     as CardanoApiShelley
import qualified Codec.Serialise         as CodecSerialise (Serialise, deserialise, serialise)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Short   as BSS
import qualified Flat                    (DecodeException, Flat (..), flat, unflat)
import qualified Ledger                  as LedgerValue
import qualified Ledger.Address          as LedgerAddress (Address)
import qualified Ledger.Value            as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusCore
import qualified PlutusCore.Pretty       as PrettyConst
import qualified PlutusTx
import qualified PlutusTx.Builtins       as TxBuiltins
import qualified PlutusTx.Code           as PlutusTxCode
import           PlutusTx.Prelude        hiding (unless)
import qualified Prelude                 as P
import qualified System.FilePath         as SystemFilePath
import qualified System.FilePath.Posix   as SystemFilePathPosix

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2
import qualified Generic.OffChainHelpers as OffChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

deployValidator :: P.String -> P.String -> LedgerApiV2.Validator -> P.IO (Either (CardanoApi.FileError ()) ())
deployValidator path file validator = do
    OffChainHelpers.writeValidator path (file ++ ".plutus") validator

deployValidatorHash :: P.String -> P.String -> LedgerApiV2.ValidatorHash -> P.IO ()
deployValidatorHash path file hash = do
    OffChainHelpers.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".hash") hash

deployValidatorAddress :: P.String -> P.String -> LedgerAddress.Address -> P.IO ()
deployValidatorAddress path file address = do
    _ <- OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> file ++ "-HEX.addr") address
    _ <- OffChainHelpers.writeFile (path SystemFilePathPosix.</> file ++ "-Mainnet.addr") $ OffChainHelpers.validatorAddrToAddrBech32Mainnet address
    _ <- OffChainHelpers.writeFile (path SystemFilePathPosix.</> file ++ "-Testnet.addr") $ OffChainHelpers.validatorAddrToAddrBech32Testnet address
    P.putStrLn $ "Addr: " ++ P.show address

--------------------------------------------------------------------------------2

deployMintingPolicy :: P.String -> P.String -> LedgerApiV2.MintingPolicy -> LedgerApiV2.CurrencySymbol -> P.IO ()
deployMintingPolicy path file policy curSymbol = do
    _ <- OffChainHelpers.writeMintingPolicy path (file ++ ".plutus") policy
    OffChainHelpers.writePlutusDataToFile (path SystemFilePathPosix.</> file ++ ".symbol") curSymbol
    P.putStrLn $ "Policy_CS: " ++ P.show curSymbol

--------------------------------------------------------------------------------2

readMintingPolicy :: P.String -> P.String -> P.IO (LedgerApiV2.MintingPolicy, LedgerApiV2.CurrencySymbol)
readMintingPolicy path file = do
    policy' <- OffChainHelpers.readMintingPolicy path (file ++ ".plutus")
    case policy' of
        Left _ -> P.putStrLn "Policy not found" >> error ()
        Right policy -> do
            let curSymbol = OffChainHelpers.getCurSymbolOfPolicy policy
            return (policy, curSymbol)

--------------------------------------------------------------------------------2

-- TODO mover a offchain helpers
serializableToScript :: CodecSerialise.Serialise a => a -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2
serializableToScript = CardanoApiShelley.PlutusScriptSerialised . BSS.toShort . BSL.toStrict . CodecSerialise.serialise

-- Serialize compiled code
codeToScript :: PlutusTx.CompiledCode a -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2
codeToScript = serializableToScript . LedgerApiV2.fromCompiledCode

-- Create file with Plutus script
writeScriptToFile :: SystemFilePath.FilePath -> CardanoApi.PlutusScript CardanoApi.PlutusScriptV2 -> P.IO ()
writeScriptToFile filePath script =
  CardanoApiShelley.writeFileTextEnvelope filePath Nothing script >>= \case
    Left err -> P.print $ CardanoApiShelley.displayError err
    Right () -> P.putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: SystemFilePath.FilePath -> PlutusTx.CompiledCode a -> P.IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

-- Create file with compiled code
writeCompiledCodeToFile
  :: SystemFilePath.FilePath
  -> PlutusTxCode.CompiledCode a
  -> P.IO ()
writeCompiledCodeToFile filePath code =
  let plcProgram = PlutusTxCode.getPlc code
      serialized = Flat.flat plcProgram
  in BS.writeFile filePath serialized

readCompiledCodeFromFile
  :: SystemFilePath.FilePath
  -> P.IO (PlutusTxCode.CompiledCode a)
readCompiledCodeFromFile filePath = do
    serialized <- BS.readFile filePath
    case Flat.unflat serialized of
        Left err   -> P.error $ "Decoding error: " ++ P.show err -- Throws an error
        Right code -> return code

--------------------------------------------------------------------------------2

{-# INLINEABLE applyPlutononyToValidatorCode #-}
applyPlutononyToValidatorCode :: PlutusTxCode.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.Validator
applyPlutononyToValidatorCode code  =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript code

{-# INLINEABLE applyPlutononyToMintingPolicyCode #-}
applyPlutononyToMintingPolicyCode :: PlutusTxCode.CompiledCode (BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.MintingPolicy
applyPlutononyToMintingPolicyCode code  =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript code

{-# INLINEABLE applyPlutononyToValidator #-}
applyPlutononyToValidator ::  (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.Validator
applyPlutononyToValidator validator  =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $$( PlutusTx.compile [|| validator ||])
    -- LedgerApiV2.mkValidatorScript $$( PlutusTx.compile [|| validator ||])

{-# INLINEABLE applyPlutononyToMintingPolicy #-}
applyPlutononyToMintingPolicy ::  (BuiltinData -> BuiltinData -> ()) -> LedgerApiV2.MintingPolicy
applyPlutononyToMintingPolicy policy  =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $$( PlutusTx.compile [|| policy ||])

--------------------------------------------------------------------------------2

dataToBuiltinData :: PlutusTx.Data -> BuiltinData
dataToBuiltinData  = TxBuiltins.dataToBuiltinData

stringHexToBuiltinData :: P.String -> BuiltinData
stringHexToBuiltinData params  =
    TxBuiltins.mkB $ OffChainHelpers.builtinByteStringFromHexString $ OffChainHelpers.bytesFromHex $ OffChainHelpers.stringToStrictByteString params



intToBuiltinData :: Integer -> BuiltinData
intToBuiltinData   = TxBuiltins.mkI

paramsIndexToBuiltinData :: [P.String] -> Integer -> BuiltinData
paramsIndexToBuiltinData params index =
    let (paramType, paramValue) = P.span (P./= ':') (params!!index) in
    case paramType of
        "string" ->  stringHexToBuiltinData $ P.drop 1 paramValue
        "bigint" ->  intToBuiltinData $ P.read $ P.drop 1 paramValue
        _        -> P.error "Unsupported type"

paramsToDataShow :: [P.String] -> Integer -> P.IO ()
paramsToDataShow params index =
    let (paramType, paramValue) = P.span (P./= ':') (params!!index) in
    case paramType of
        "string" -> P.putStrLn $ P.show index ++ "params string: " ++ P.drop 1 paramValue
        "bigint" ->  P.putStrLn $ P.show index ++ "params bigint: " ++ P.drop 1 paramValue
        _        -> P.error "Unsupported type"

optimizeValidator :: SystemFilePath.FilePath -> SystemFilePath.FilePath -> [P.String] -> P.IO ()
optimizeValidator contractFilePath optimizedContractSaveFileName scriptParams = do
    let dirPath = SystemFilePath.takeDirectory contractFilePath
    P.putStrLn $ "readCompiledCodeFromFile: " ++ contractFilePath

    compileCodeWithParams <- case length scriptParams of
        0 -> readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
        1 -> do
            code <- readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            paramsToDataShow scriptParams 0
            return $ code `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
        2 -> do
            code <- readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
        3 -> do
            code <- readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
        4 -> do
            code <- readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
        5 -> do
            code <- readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3
            _ <- paramsToDataShow scriptParams 4
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 4)
        _ -> P.error "Too Many Params"

    P.putStrLn "applyPlutononyToValidatorCode..."
    let validator = applyPlutononyToValidatorCode compileCodeWithParams
    P.putStrLn $ "writeValidator: " ++ (dirPath SystemFilePathPosix.</> optimizedContractSaveFileName)
    _ <- OffChainHelpers.writeValidator dirPath optimizedContractSaveFileName validator
    return ()

--------------------------------------------------------------------------------2

optimizeMintingPolicy :: SystemFilePath.FilePath -> SystemFilePath.FilePath -> [P.String] -> P.IO ()
optimizeMintingPolicy contractFilePath optimizedContractSaveFileName scriptParams = do
    let dirPath = SystemFilePath.takeDirectory contractFilePath
    P.putStrLn $ "readCompiledCodeFromFile: " ++ contractFilePath

    compileCodeWithParams <- case length scriptParams of
        0 -> readCompiledCodeFromFile @(BuiltinData ->  BuiltinData -> ()) contractFilePath
        1 -> do
            code <- readCompiledCodeFromFile @(BuiltinData ->  BuiltinData -> BuiltinData -> ()) contractFilePath
            paramsToDataShow scriptParams 0
            return $ code `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
        2 -> do
            code <- readCompiledCodeFromFile @(BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
        3 -> do
            code <- readCompiledCodeFromFile @(BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
        4 -> do
            code <- readCompiledCodeFromFile @(BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3

            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
        5 -> do
            code <- readCompiledCodeFromFile @(BuiltinData -> BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()) contractFilePath
            _ <- paramsToDataShow scriptParams 0
            _ <- paramsToDataShow scriptParams 1
            _ <- paramsToDataShow scriptParams 2
            _ <- paramsToDataShow scriptParams 3
            _ <- paramsToDataShow scriptParams 4
            return $ code
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 0)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 1)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 2)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 3)
                `PlutusTx.applyCode` PlutusTx.liftCode (paramsIndexToBuiltinData scriptParams 4)
        _ -> P.error "Too Many Params"

    P.putStrLn "applyPlutononyToMintingPolicyCode..."
    let policy = applyPlutononyToMintingPolicyCode compileCodeWithParams
    P.putStrLn $ "writeMintingPolicy: " ++ (dirPath SystemFilePathPosix.</> optimizedContractSaveFileName)
    _ <- OffChainHelpers.writeMintingPolicy dirPath optimizedContractSaveFileName policy
    return ()

--------------------------------------------------------------------------------2
