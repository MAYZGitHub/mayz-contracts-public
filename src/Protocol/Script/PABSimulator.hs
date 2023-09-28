--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Script.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class as MonadIOClass (MonadIO (..))
import qualified Plutus.PAB.Simulator   as PABSimulator
import           PlutusTx.Prelude       hiding (unless)
import qualified Prelude                as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.PABHelpers     as PABHelpers
import qualified Protocol.PABContracts  as PABContracts
import qualified Protocol.PABTypes      as T
import qualified Generic.CLIHelpers as CLIHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

pabScriptAddInProtocol :: PABContracts.PABParamsInProtocolScriptMenu
pabScriptAddInProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToProtocol pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Add Scripts"
    let contract =
            PABContracts.PABProtocolScriptAdd
                T.PABProtocolScriptAddParams
                    { T.ppsapProtocolPABParams = protocolPABParams
                    }
    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabReturnToProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

----------------------------------------------------------------------------

pabScriptDeleteInProtocol :: PABContracts.PABParamsInProtocolScriptMenu
pabScriptDeleteInProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabReturnToProtocol pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Delete Scripts"
    let contract =
            PABContracts.PABProtocolScriptDelete
                T.PABProtocolScriptDeleteParams
                    { T.ppsdpProtocolPABParams = protocolPABParams
                    }

    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabReturnToProtocol (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabScriptAddInFund :: PABContracts.PABParamsInFundScriptMenu
pabScriptAddInFund (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabReturnToFundMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Add Scripts"
    case fundPABParams' of
        (Just fundPABParams) -> do
            let contract =
                    PABContracts.PABFundScriptAdd
                        T.PABFundScriptAddParams
                            { T.pfsapProtocolPABParams = protocolPABParams,
                              T.pfsapFundPABParams = fundPABParams
                            }
            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

----------------------------------------------------------------------------

pabScriptDeleteInFund :: PABContracts.PABParamsInFundScriptMenu
pabScriptDeleteInFund (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabReturnToFundMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Delete Scripts"
    case fundPABParams' of
        (Just fundPABParams) -> do
            let contract =
                    PABContracts.PABFundScriptDelete
                        T.PABFundScriptDeleteParams
                            { T.pfsdpProtocolPABParams = protocolPABParams,
                              T.pfsdpFundPABParams = fundPABParams
                            }

            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
            pabReturnToFundMenu True (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

----------------------------------------------------------------------------
