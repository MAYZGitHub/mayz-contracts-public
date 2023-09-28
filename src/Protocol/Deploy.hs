{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Deploy where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Control.Monad.IO.Class        as MonadIOClass (MonadIO (..))
import qualified Ledger
import qualified Ledger.Crypto                 as Crypto
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude              hiding (unless)
import qualified Prelude                       as P
import qualified System.Directory              as SystemDirectory
import qualified System.FilePath               as SystemFilePath
import qualified System.FilePath.Posix         as SystemFilePathPosix

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.CLIHelpers            as CLIHelpers
import qualified Generic.DeployHelpers         as DeployHelpers
import qualified Generic.OffChainHelpers       as OffChainHelpers
import qualified Generic.OffChainHelpers       as Utils
import qualified Generic.Types                 as T
import qualified Protocol.BuyOrder.OnChain     as BuyOrderOnChain
import qualified Protocol.BuyOrder.Types       as BuyOrderT
import qualified Protocol.Constants            as T
import qualified Protocol.Delegation.OnChain   as DelegationOnChain
import qualified Protocol.Delegation.Types     as DelegationT
import qualified Protocol.Fund.Holding.OnChain as FundHoldingOnChain
import qualified Protocol.Fund.Holding.Types   as FundHoldingT
import qualified Protocol.Fund.OnChain         as FundOnChain
import qualified Protocol.Fund.Types           as FundT
import qualified Protocol.InvestUnit.OnChain   as InvestUnitOnChain
import qualified Protocol.InvestUnit.Types     as InvestUnitT
import qualified Protocol.PABTypes             as T
import qualified Protocol.Protocol.OnChain     as ProtocolOnChain
import qualified Protocol.Protocol.Types       as ProtocolT
import qualified Protocol.Script.OnChain       as ScriptOnChain
import qualified Protocol.Script.Types         as ScriptT
import qualified Protocol.SellOffer.OnChain    as SellOfferOnChain
import qualified Protocol.SellOffer.Types      as SellOfferT

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

-- Para obtener pab file de un protocolo especifico con una fabrica de fondo incluida
deploy_ProtocolPABParams_With_RequestingParams :: LedgerApiV2.TxOutRef -> P.IO T.ProtocolPABParams
deploy_ProtocolPABParams_With_RequestingParams protocolPolicyID_TxOutRef = do
    -- MonadIOClass.liftIO $ P.putStrLn "Path (default=export/protocol):"
    -- !path <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "export/protocol"
    let path = "export/protocol-v" ++ P.show T.protocolFactoryVersion
    MonadIOClass.liftIO $ P.putStrLn "Protocol Name (default=demo-protocol):"
    !protocolName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "demo-protocol"
    deploy_ProtocolPABParams protocolPolicyID_TxOutRef path protocolName

-- Para obtener pab file de un protocolo especifico con una fabrica de fondo incluida
deploy_ProtocolPABParams :: LedgerApiV2.TxOutRef -> P.FilePath -> P.String -> P.IO T.ProtocolPABParams
deploy_ProtocolPABParams protocolPolicyID_TxOutRef path protocolName = do
    SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> protocolName)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> protocolName)
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating Protocol PAB Params..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> protocolName
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol PolicyID' Script..."
    MonadIOClass.liftIO $ P.putStrLn $ "Protocol PolicyID TxOutRef: " ++ P.show protocolPolicyID_TxOutRef
    ------------------------------
    let protocolPolicyParams =
            ProtocolT.PolicyParams
                { ProtocolT.ppProtocolPolicyID_TxOutRef = protocolPolicyID_TxOutRef
                }
        protocolPolicyID = ProtocolOnChain.policyID protocolPolicyParams
        protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ProtocolPolicyID" protocolPolicyID protocolPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol Validator' Script..."
    let protocolValidatorParams =
            ProtocolT.ValidatorParams
                    { ProtocolT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    }
        protocolValidator = ProtocolOnChain.validator protocolValidatorParams
        protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator
        protocolValidator_Address = OffChainHelpers.addressValidator protocolValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script PolicyID' Script..."
    let scriptPolicyParams =
            ScriptT.PolicyParams
                    { ScriptT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                    }
        scriptPolicyID = ScriptOnChain.policyID scriptPolicyParams
        scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ScriptPolicyID" scriptPolicyID scriptPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script Validator' Script..."
    let scriptValidatorParams =
            ScriptT.ValidatorParams
                    { ScriptT.vpScriptPolicyID_CS = scriptPolicyID_CS,
                        ScriptT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    }
        scriptValidator = ScriptOnChain.validator scriptValidatorParams
        scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator
        scriptValidator_Address = OffChainHelpers.addressValidator scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Validator' Script..."
    let fundValidatorParams =
            FundT.ValidatorParams
                { FundT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                }
        fundValidator = FundOnChain.validator fundValidatorParams
        fundValidator_Hash = OffChainHelpers.hashValidator fundValidator
        fundValidator_Address = OffChainHelpers.addressValidator fundValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "FundValidator" fundValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "FundValidator" fundValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "FundValidator" fundValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'InvestUnit Validator' Script..."
    let investUnitValidatorParams =
            InvestUnitT.ValidatorParams
                { InvestUnitT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                }
        investUnitValidator = InvestUnitOnChain.validator investUnitValidatorParams
        investUnitValidator_Hash = OffChainHelpers.hashValidator investUnitValidator
        investUnitValidator_Address = OffChainHelpers.addressValidator investUnitValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "InvestUnitValidator" investUnitValidator_Address
    ------------------------------
    let fundFactoryPABParams =
            T.FundFactoryPABParams
            {
                T.ffppFundFactoryVersion       = T.fundFactoryVersion,
                T.ffppFundValidator_Params = fundValidatorParams,
                T.ffppFundValidator = fundValidator,
                T.ffppFundValidator_Hash = fundValidator_Hash,
                T.ffppFundValidator_Address = fundValidator_Address,
                T.ffppInvestUnitValidator_Params = investUnitValidatorParams,
                T.ffppInvestUnitValidator = investUnitValidator,
                T.ffppInvestUnitValidator_Hash = investUnitValidator_Hash,
                T.ffppInvestUnitValidator_Address = investUnitValidator_Address,
                T.ffppFundPABParams    =  []
            }
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Creating 'Protocol PAB Params' File..."
    ------------------------------
    let protocolPABParams =
            T.ProtocolPABParams {
                T.pppProtocolFactoryVersion = T.protocolFactoryVersion,
                T.pppProtocolPolicyID_Params = protocolPolicyParams,
                T.pppProtocolPolicyID = protocolPolicyID,
                T.pppProtocolPolicyID_CS = protocolPolicyID_CS,
                T.pppProtocolValidator_Params = protocolValidatorParams,
                T.pppProtocolValidator = protocolValidator,
                T.pppProtocolValidator_Hash = protocolValidator_Hash,
                T.pppProtocolValidator_Address = protocolValidator_Address,
                T.pppScriptPolicyID_Params = scriptPolicyParams,
                T.pppScriptPolicyID = scriptPolicyID,
                T.pppScriptPolicyID_CS = scriptPolicyID_CS,
                T.pppScriptValidator_Params = scriptValidatorParams,
                T.pppScriptValidator = scriptValidator,
                T.pppScriptValidator_Hash = scriptValidator_Hash,
                T.pppScriptValidator_Address = scriptValidator_Address,
                T.pppFundFactoryPABParams = [fundFactoryPABParams]
            }
    OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPAB.json") protocolPABParams
    P.putStrLn $ "Saved Protocol PAB Param in: " ++ P.show (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPAB.json")
    ------------------------------
    P.putStrLn "--------------------------------"
    ------------------------------
    return protocolPABParams

--------------------------------------------------------------------------------

-- Para obtener pab file de un fondo especifico
deploy_FundPABParams_With_RequestingParams :: T.CS ->  LedgerApiV2.TxOutRef  -> P.IO T.FundPABParams
deploy_FundPABParams_With_RequestingParams protocolPolicyID_CS  fundPolicy_TxOutRef  = do
    let path = "export/funds-v" ++ P.show T.fundFactoryVersion
    MonadIOClass.liftIO $ P.putStrLn "Fund Name (default=demo-fund):"
    !fundName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "demo-fund"
    ------------------------------
    deploy_FundPABParams protocolPolicyID_CS fundPolicy_TxOutRef path fundName

-- Para obtener pab file de un fondo especifico
deploy_FundPABParams :: T.CS -> LedgerApiV2.TxOutRef -> P.FilePath -> P.String -> P.IO T.FundPABParams
deploy_FundPABParams protocolPolicyID_CS fundPolicy_TxOutRef path fundName =
    do
        SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> fundName)
        SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> fundName)
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating Fund PAB Params..."
        ------------------------------
        P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> fundName
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Validator' Script..."
        let fundValidatorParams =
                FundT.ValidatorParams
                    { FundT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    }
            fundValidator = FundOnChain.validator fundValidatorParams
            fundValidator_Hash = OffChainHelpers.hashValidator fundValidator
            fundValidator_Address = OffChainHelpers.addressValidator fundValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "FundValidator" fundValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Policy' Script..."
        let fundPolicyParams =
                FundT.PolicyParams
                    { FundT.ppProtocolPolicyID_CS = protocolPolicyID_CS,
                        FundT.ppFundPolicy_TxOutRef = fundPolicy_TxOutRef,
                        FundT.ppFundValidator_Hash = fundValidator_Hash
                    }
            fundPolicy = FundOnChain.policy fundPolicyParams
            fundPolicy_CS = OffChainHelpers.getCurSymbolOfPolicy fundPolicy
        _ <- MonadIOClass.liftIO $ P.print fundPolicyParams
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> fundName) "FundPolicy" fundPolicy fundPolicy_CS
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Holding PolicyID' Script..."
        let fundHoldingPolicyParams =
                FundHoldingT.PolicyParams
                    { FundHoldingT.ppFundPolicy_CS = fundPolicy_CS
                    }
            fundHoldingPolicyID = FundHoldingOnChain.policyID fundHoldingPolicyParams
            fundHoldingPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy fundHoldingPolicyID
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> fundName) "FundHoldingPolicyID" fundHoldingPolicyID fundHoldingPolicyID_CS
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Fund Holding Validator' Script..."
        let fundHoldingValidatorParams =
                FundHoldingT.ValidatorParams
                        { FundHoldingT.vpProtocolPolicyID_CS = protocolPolicyID_CS,
                            FundHoldingT.vpFundPolicy_CS = fundPolicy_CS
                        }
            fundHoldingValidator = FundHoldingOnChain.validator fundHoldingValidatorParams
            fundHoldingValidator_Hash = OffChainHelpers.hashValidator fundHoldingValidator
            fundHoldingValidator_Address = OffChainHelpers.addressValidator fundHoldingValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "FundHoldingValidator" fundHoldingValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'InvestUnit Validator' Script..."
        let investUnitValidatorParams =
                InvestUnitT.ValidatorParams
                    { InvestUnitT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    }
            investUnitValidator = InvestUnitOnChain.validator investUnitValidatorParams
            investUnitValidator_Hash = OffChainHelpers.hashValidator investUnitValidator
            investUnitValidator_Address = OffChainHelpers.addressValidator investUnitValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> fundName) "InvestUnitValidator" investUnitValidator
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> fundName) "InvestUnitValidator" investUnitValidator_Hash
        _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> fundName) "InvestUnitValidator" investUnitValidator_Address
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Creating 'Fund PAB Params' File..."
        let fundPABParams =
                T.FundPABParams {
                    T.fppFundFactoryVersion = T.fundFactoryVersion,
                    T.fppFundPolicy_Params = fundPolicyParams,
                    T.fppFundPolicy = fundPolicy,
                    T.fppFundPolicy_CS = fundPolicy_CS,
                    T.fppFundValidator_Params = fundValidatorParams,
                    T.fppFundValidator = fundValidator,
                    T.fppFundValidator_Hash = fundValidator_Hash,
                    T.fppFundValidator_Address = fundValidator_Address,
                    T.fppInvestUnitValidator_Params = investUnitValidatorParams,
                    T.fppInvestUnitValidator = investUnitValidator,
                    T.fppInvestUnitValidator_Hash = investUnitValidator_Hash,
                    T.fppInvestUnitValidator_Address = investUnitValidator_Address,
                    T.fppFundHoldingPolicyID_Params = fundHoldingPolicyParams,
                    T.fppFundHoldingPolicyID = fundHoldingPolicyID,
                    T.fppFundHoldingPolicyID_CS = fundHoldingPolicyID_CS,
                    T.fppFundHoldingValidator_Params = fundHoldingValidatorParams,
                    T.fppFundHoldingValidator = fundHoldingValidator,
                    T.fppFundHoldingValidator_Hash = fundHoldingValidator_Hash,
                    T.fppFundHoldingValidator_Address = fundHoldingValidator_Address
                }
        OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPAB.json") fundPABParams
        P.putStrLn $ "Saved Fund PAB Param in: " ++ P.show (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPAB.json")
        ------------------------------
        return fundPABParams

--------------------------------------------------------------------------------2


