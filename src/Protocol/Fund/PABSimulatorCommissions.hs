--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
{-# LANGUAGE TypeApplications #-}
module Protocol.Fund.PABSimulatorCommissions where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Control.Monad                as Monad
import qualified Control.Monad.IO.Class       as MonadIOClass (MonadIO (..))
import qualified Data.Aeson                   as DataAeson (decode)
import qualified Data.Maybe                   as DataMaybe
import qualified Ledger
import qualified Plutus.PAB.Simulator         as PABSimulator
import           PlutusTx.Prelude             hiding (unless)
import qualified Prelude                      as P
import qualified System.Directory             as SystemDirectory
import qualified System.FilePath.Posix        as SystemFilePathPosix
import qualified Text.Read                    as TextRead (readMaybe)
--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2
import qualified Generic.CLIHelpers           as CLIHelpers
import qualified Generic.CLIHelpers           as HelpersCLI
import qualified Generic.OffChainHelpers      as OffChainHelpers
import qualified Generic.PABHelpers           as PABHelpers
import qualified Protocol.Deploy              as Deploy
import qualified Protocol.Fund.Holding.Types  as FundHoldingT
import qualified Protocol.Fund.PABSimulator   as FundPABSimulator
import qualified Protocol.PABContracts        as PABContracts
import qualified Protocol.PABHelpers          as PABHelpers
import qualified Protocol.PABTypes            as T
import qualified Protocol.Script.PABSimulator as ScriptPABSimulator

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

menuEndPoints :: P.String -> PABContracts.PABParamsInFundMenu' P.String
menuEndPoints name _ (walletNro, _) protocolPABParams fundPABParams' _ _  = do
    ---------------------
    !isCoreTeam <- return $ PABHelpers.isCoreTeam (Just walletNro)
    !isProtocolAdmin <- PABHelpers.isProtocolAdmin (Just walletNro) (Just protocolPABParams)
    !isMAYZHolderAdmin <- PABHelpers.isMAYZHolderAdmin (Just walletNro) (Just protocolPABParams)
    !isFundAdmin <- PABHelpers.isFundAdmin (Just walletNro) (Just protocolPABParams)
    !isThisFundAdmin <- PABHelpers.isThisFundAdmin (Just walletNro) (Just protocolPABParams) fundPABParams'
    !isMAYZHolder <- PABHelpers.isMAYZHolder (Just walletNro)
    ---------------------
    MonadIOClass.liftIO $ CLIHelpers.printTitle $ "FUNDS COMMISSIONS MENU - " ++ name
    MonadIOClass.liftIO $ P.putStrLn $ "isCoreTeam: " ++ P.show isCoreTeam ++ " - isProtocolAdmin: " ++ P.show isProtocolAdmin ++ " - isMAYZHolderAdmin: " ++ P.show isMAYZHolderAdmin ++ " - isFundAdmin: " ++ P.show isFundAdmin ++ " - isThisFundAdmin: " ++ P.show isThisFundAdmin ++ " - isMAYZHolder: " ++ P.show isMAYZHolder
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn $ "There are " ++ P.show (length $ T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams) ++ " Fund(s)"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    case fundPABParams' of
        Nothing -> do
            MonadIOClass.liftIO $ P.putStrLn "Please select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        Just fundPABParams -> do
            MonadIOClass.liftIO $ P.putStrLn $ "Selected Fund: " ++ P.show (T.fppFundPolicy_CS fundPABParams) ++ ")"
            MonadIOClass.liftIO $ P.putStrLn "11: Invest Unit | 12: FT Price | 13: Token Prices"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "21 - Select Fund"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    Monad.when (DataMaybe.isJust fundPABParams' && (isProtocolAdmin || isMAYZHolderAdmin || isThisFundAdmin)) $ do
        Monad.when isProtocolAdmin $ do MonadIOClass.liftIO $ P.putStrLn "31 - Withdraw Protocol commissions"
        Monad.when isMAYZHolderAdmin $ do MonadIOClass.liftIO $ P.putStrLn "32 - Withdraw MAYZ Holder commissions"
        Monad.when isThisFundAdmin $ do MonadIOClass.liftIO $ P.putStrLn "33 - Withdraw Fund commissions"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "0  - Return to Main Menu"
    MonadIOClass.liftIO $ P.putStrLn "99 - Exit"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "Enter option:"
    option <- MonadIOClass.liftIO P.getLine
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    return option

--------------------------------------------------------------------------------2

pabMainLoop :: PABContracts.PABParamsInFundMenu
pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    let name = "Protocol: " ++ P.show (T.pppProtocolPolicyID_CS protocolPABParams)
    option <- menuEndPoints name isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
    case option of
        "11" -> do
            PABHelpers.pabShowInvestUnit isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "21" ->
            pabSelectFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "31" ->
            pabFundCollect_Protocol_Commissions isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "32" ->
            pabFundCollect_MAYZ_Commissions isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "33" ->
            pabFundCollect_FundAdmins_Commissions isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "81" -> do
            PABHelpers.pabBalances (Just walletNro, walletCount) (Just protocolPABParams)
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "82" -> do
            PABHelpers.pabUTxOAtWallet (Just walletNro, walletCount)
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "83" -> do
            PABHelpers.pabUTxOAtScript (Just protocolPABParams)
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "91" -> do
            PABHelpers.pabTimeAndSlot
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "0" -> do
            pabReturnToMainMenu (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
        "99" -> do
            MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Shutting Down PAB Simulator"
            PABHelpers.pabBalances (Just walletNro, walletCount) (Just protocolPABParams)
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            MonadIOClass.liftIO $ P.putStrLn "Exiting now..."
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid option"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabSelectFundParams :: PABContracts.PABParamsInFundMenu
pabSelectFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Select Fund"
    !fundPABParamsSelected' <- MonadIOClass.liftIO $ CLIHelpers.selectFromList (T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams)
    case fundPABParamsSelected' of
        Nothing -> do
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        Just fundPABParamsSelected -> do
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams (Just $ snd fundPABParamsSelected) pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundCollect_Protocol_Commissions :: PABContracts.PABParamsInFundMenu
pabFundCollect_Protocol_Commissions isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Withdraw Protocol Commissions"
    case fundPABParams' of
        (Just fundPABParams) -> do
            ----------------
            !blockchain <- PABSimulator.blockchain
            let
                !address = T.fppFundHoldingValidator_Address fundPABParams
                !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address
                !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
            MonadIOClass.liftIO $ P.putStrLn "Select Holding to use"
            selectedUTxO <- PABHelpers.selectUTxOWithDatumAndCS @FundHoldingT.ValidatorDatum fundHoldingPolicyID_CS uTxOuts blockchain
            ----------------
            case selectedUTxO of
                Nothing -> do
                    PABHelpers.waitKeyPress
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                Just (_, txOutRef, _) -> do
                    amount <-  MonadIOClass.liftIO $ HelpersCLI.getIntWithDefault "Withdraw FT" 1_000_000
                    let
                        contract = PABContracts.PABFundCollect_Protocol_Commissions T.PABFundCollect_Protocol_CommissionsParams{
                              T.pfwpcpProtocolPABParams = protocolPABParams
                            , T.pfwpcpFundPABParams = fundPABParams
                            , T.pfwpcpAmount   = amount,
                              T.pfwpcpFundHoldingTxOutRef = txOutRef
                        }
                    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
                    PABHelpers.waitContractAndKeyPress contractInstance
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundCollect_MAYZ_Commissions :: PABContracts.PABParamsInFundMenu
pabFundCollect_MAYZ_Commissions isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Withdraw MAYZ Commissions"
    case fundPABParams' of
        (Just fundPABParams) -> do
            ----------------
            !blockchain <- PABSimulator.blockchain
            let
                !address = T.fppFundHoldingValidator_Address fundPABParams
                !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address
                !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
            MonadIOClass.liftIO $ P.putStrLn "Select Holding to use"
            selectedUTxO <- PABHelpers.selectUTxOWithDatumAndCS @FundHoldingT.ValidatorDatum fundHoldingPolicyID_CS uTxOuts blockchain
            ----------------
            case selectedUTxO of
                Nothing -> do
                    PABHelpers.waitKeyPress
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                Just (_, txOutRef, _) -> do
                    amount <-  MonadIOClass.liftIO $ HelpersCLI.getIntWithDefault "Withdraw FT" 1_000_000
                    let
                        contract = PABContracts.PABFundCollect_MAYZ_Commissions T.PABFundCollect_MAYZ_CommissionsParams{
                              T.pfwmcpProtocolPABParams = protocolPABParams
                            , T.pfwmcpFundPABParams = fundPABParams
                            , T.pfwmcpAmount   = amount,
                              T.pfwmcpFundHoldingTxOutRef = txOutRef
                        }
                    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
                    PABHelpers.waitContractAndKeyPress contractInstance
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundCollect_FundAdmins_Commissions :: PABContracts.PABParamsInFundMenu
pabFundCollect_FundAdmins_Commissions isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Withdraw Fund Admins Commissions"
    case fundPABParams' of
        (Just fundPABParams) -> do
            ----------------
            !blockchain <- PABSimulator.blockchain
            let
                !address = T.fppFundHoldingValidator_Address fundPABParams
                !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address
                !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
            MonadIOClass.liftIO $ P.putStrLn "Select Holding to use"
            selectedUTxO <- PABHelpers.selectUTxOWithDatumAndCS @FundHoldingT.ValidatorDatum fundHoldingPolicyID_CS uTxOuts blockchain
            ----------------
            case selectedUTxO of
                Nothing -> do
                    PABHelpers.waitKeyPress
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                Just (_, txOutRef, _) -> do
                    amount <-  MonadIOClass.liftIO $ HelpersCLI.getIntWithDefault "Withdraw FT" 1_000_000
                    let
                        contract = PABContracts.PABFundCollect_FundAdmins_Commissions T.PABFundCollect_FundAdmins_CommissionsParams{
                              T.pfwfcpProtocolPABParams = protocolPABParams
                            , T.pfwfcpFundPABParams = fundPABParams
                            , T.pfwfcpAmount   = amount,
                              T.pfwfcpFundHoldingTxOutRef = txOutRef
                        }
                    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
                    PABHelpers.waitContractAndKeyPress contractInstance
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "You must select a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

