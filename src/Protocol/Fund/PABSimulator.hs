--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
{-# LANGUAGE TypeApplications #-}
module Protocol.Fund.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class       as MonadIOClass (MonadIO (..))
import qualified Data.Aeson                   as DataAeson (decode)
import qualified Ledger
import qualified Plutus.PAB.Simulator         as PABSimulator
import           PlutusTx.Prelude             hiding (unless)
import qualified Prelude                      as P
import qualified System.Directory             as SystemDirectory
import qualified System.FilePath.Posix        as SystemFilePathPosix
import qualified Text.Read                    as TextRead (readMaybe)
import qualified Control.Monad.Freer as MonadFreerInternal

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2
import qualified Generic.CLIHelpers           as CLIHelpers
import qualified Generic.OffChainHelpers      as OffChainHelpers
import qualified Generic.PABHelpers           as PABHelpers
import qualified Protocol.Deploy              as Deploy
import qualified Protocol.Fund.Holding.Types           as FundHoldingT
import qualified Protocol.PABContracts        as PABContracts
import qualified Protocol.PABTypes            as T
import qualified Protocol.Script.PABSimulator as ScriptPABSimulator
import qualified Protocol.InvestUnit.Types as InvestUnitT
import qualified Protocol.PABHelpers as PABHelpers
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Control.Concurrent.STM as ConcurrentSTM
import qualified Cardano.Node.Emulator as LedgerTimeSlot
import qualified Data.Default as DataDefault
import qualified Ledger.Value as LedgerValue
import qualified Data.Maybe as DataMaybe
import qualified Protocol.Constants as T
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Control.Monad as Monad
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

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
    MonadIOClass.liftIO $ CLIHelpers.printTitle $ "FUNDS ADMIN MENU - " ++ name
    MonadIOClass.liftIO $ P.putStrLn $ "isCoreTeam: " ++ P.show isCoreTeam ++ " - isProtocolAdmin: " ++ P.show isProtocolAdmin ++ " - isMAYZHolderAdmin: " ++ P.show isMAYZHolderAdmin ++ " - isFundAdmin: " ++ P.show isFundAdmin ++ " - isThisFundAdmin: " ++ P.show isThisFundAdmin ++ " - isMAYZHolder: " ++ P.show isMAYZHolder
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn $ "There are " ++ P.show (length $ T.ffppFundPABParams $ head $ T.pppFundFactoryPABParams protocolPABParams) ++ " Fund(s)"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    case fundPABParams' of
        Nothing -> do
            MonadIOClass.liftIO $ P.putStrLn "Please create, select or load a Fund"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        Just fundPABParams -> do
            MonadIOClass.liftIO $ P.putStrLn $ "Selected Fund: " ++ P.show (T.fppFundPolicy_CS fundPABParams) ++ ")"
            MonadIOClass.liftIO $ P.putStrLn "11: Invest Unit | 12: FT Price | 13: Token Prices"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    MonadIOClass.liftIO $ P.putStrLn "21 - New Fund"
    MonadIOClass.liftIO $ P.putStrLn "22 - Select Fund"
    MonadIOClass.liftIO $ P.putStrLn "23 - Load Fund"
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    Monad.when (DataMaybe.isJust fundPABParams') $ do
        MonadIOClass.liftIO $ P.putStrLn "31 - Fund Prepare"
        MonadIOClass.liftIO $ P.putStrLn "32 - Fund Update"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "41 - Add Scripts"
        MonadIOClass.liftIO $ P.putStrLn "42 - Delete Scripts"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "51 - Add Holdings"
        MonadIOClass.liftIO $ P.putStrLn "52 - Delete Holdings"
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "61 - Re-Indexing"
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
    option <- menuEndPoints name isAdminMenu(walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
    case option of

        "11" -> do
            PABHelpers.pabShowInvestUnit isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
            pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        -- "12" -> do
        --     PABHelpers.pabShowFTPriceADA isAdminMenu(walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        --     pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        -- "13" -> do
        --     PABHelpers.pabShowReIdxPriceADA isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        --     pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "21" ->
            pabCreateFundParams isAdminMenu(walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "22" ->
            pabSelectFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "23" ->
            pabLoadFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "31" ->
            pabFundPrepare isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "32" ->
            pabFundUpdate isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
       
        "41" ->
            ScriptPABSimulator.pabScriptAddInFund  (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabMainLoop pabShutdown
        "42" ->
            ScriptPABSimulator.pabScriptDeleteInFund  (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabMainLoop pabShutdown

        "51" ->
            pabFundHoldingAdd isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        "52" ->
            pabFundHoldingDelete isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

        "61" ->
            pabFundReIndexing isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

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

pabCreateFundParams :: PABContracts.PABParamsInFundMenu
pabCreateFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Create Fund"
    blockchain <- PABSimulator.blockchain
    ---------------------
    let uTxOutRefAt = fst <$> PABHelpers.getUTxOsListInPABSimulator blockchain (PABHelpers.walletPaymentPubKeyHashAddress walletNro)
        fundPolicy_TxOutRef = head uTxOutRefAt
    ---------------------
    MonadIOClass.liftIO $ P.putStrLn $ "fundPolicy_TxOutRef: " ++ P.show fundPolicy_TxOutRef
    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
    ---------------------
    fundPABParams <- MonadIOClass.liftIO $ Deploy.deploy_FundPABParams_With_RequestingParams (T.pppProtocolPolicyID_CS protocolPABParams) fundPolicy_TxOutRef
    ---------------------
    newProtocolPABParams <-
        if fundPABParams `P.elem` T.ffppFundPABParams (head $ T.pppFundFactoryPABParams protocolPABParams)
            then do
                MonadIOClass.liftIO $ P.putStrLn "Fund already exists"
                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                return protocolPABParams
            else do
                MonadIOClass.liftIO $ P.putStrLn "Fund created"
                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                let
                    fundFactoryPABParams = head $ T.pppFundFactoryPABParams protocolPABParams
                    fundFactoryPABParams' =
                        T.FundFactoryPABParams
                            {
                                T.ffppFundFactoryVersion  = T.ffppFundFactoryVersion fundFactoryPABParams,
                                T.ffppFundValidator_Params = T.ffppFundValidator_Params fundFactoryPABParams,
                                T.ffppFundValidator = T.ffppFundValidator fundFactoryPABParams,
                                T.ffppFundValidator_Hash = T.ffppFundValidator_Hash fundFactoryPABParams,
                                T.ffppFundValidator_Address = T.ffppFundValidator_Address fundFactoryPABParams,
                                T.ffppInvestUnitValidator_Params = T.ffppInvestUnitValidator_Params fundFactoryPABParams,
                                T.ffppInvestUnitValidator = T.ffppInvestUnitValidator fundFactoryPABParams,
                                T.ffppInvestUnitValidator_Hash = T.ffppInvestUnitValidator_Hash fundFactoryPABParams,
                                T.ffppInvestUnitValidator_Address = T.ffppInvestUnitValidator_Address fundFactoryPABParams,
                                T.ffppFundPABParams              = fundPABParams : T.ffppFundPABParams fundFactoryPABParams
                            }
                let newProtocolPABParams =
                        T.ProtocolPABParams
                            { T.pppProtocolFactoryVersion = T.pppProtocolFactoryVersion protocolPABParams,
                            T.pppProtocolPolicyID_Params = T.pppProtocolPolicyID_Params protocolPABParams,
                            T.pppProtocolPolicyID = T.pppProtocolPolicyID protocolPABParams,
                            T.pppProtocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams,
                            T.pppProtocolValidator_Params = T.pppProtocolValidator_Params protocolPABParams,
                            T.pppProtocolValidator = T.pppProtocolValidator protocolPABParams,
                            T.pppProtocolValidator_Hash = T.pppProtocolValidator_Hash protocolPABParams,
                            T.pppProtocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams,
                            T.pppScriptPolicyID_Params = T.pppScriptPolicyID_Params protocolPABParams,
                            T.pppScriptPolicyID = T.pppScriptPolicyID protocolPABParams,
                            T.pppScriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams,
                            T.pppScriptValidator_Params = T.pppScriptValidator_Params protocolPABParams,
                            T.pppScriptValidator = T.pppScriptValidator protocolPABParams,
                            T.pppScriptValidator_Hash = T.pppScriptValidator_Hash protocolPABParams,
                            T.pppScriptValidator_Address = T.pppScriptValidator_Address protocolPABParams,
                            T.pppFundFactoryPABParams = [fundFactoryPABParams']
                            }
                return newProtocolPABParams
    PABHelpers.waitKeyPress
    pabMainLoop isAdminMenu (walletNro, walletCount) newProtocolPABParams (Just fundPABParams) pabReturnToMainMenu pabShutdown

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

pabLoadFundParams :: PABContracts.PABParamsInFundMenu
pabLoadFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Load Fund"
    -- MonadIOClass.liftIO $ P.putStrLn "Path (default=export/funds):"
    -- !path <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault "export/funds"
    let !path = "export/funds-v" ++ P.show T.fundFactoryVersion
    !existPath <- MonadIOClass.liftIO $ SystemDirectory.doesPathExist path
    if existPath
        then do
            !fundName <- MonadIOClass.liftIO $ CLIHelpers.selectFolder path ""
            if P.null fundName -- TODO: check length fundName == 0
                then do
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                else do
                    !exist <- MonadIOClass.liftIO $ SystemDirectory.doesFileExist (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPAB.json")
                    if exist
                        then do
                            !jsonFile <- MonadIOClass.liftIO $ OffChainHelpers.readFile (path SystemFilePathPosix.</> fundName SystemFilePathPosix.</> "FundPAB.json")
                            case DataAeson.decode jsonFile :: Maybe T.FundPABParams of
                                Nothing -> do
                                    MonadIOClass.liftIO $ P.putStrLn "Invalid input. Can't decode FundPAB.json file"
                                    MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                                    pabLoadFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                                Just fundPABParams -> do
                                    newProtocolPABParams <-
                                        if fundPABParams `P.elem` T.ffppFundPABParams ( head $ T.pppFundFactoryPABParams protocolPABParams)
                                            then do
                                                MonadIOClass.liftIO $ P.putStrLn "Fund already exists"
                                                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                                                return protocolPABParams
                                            else do
                                                MonadIOClass.liftIO $ P.putStrLn "Fund loaded"
                                                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                                                let
                                                    fundFactoryPABParams = head $ T.pppFundFactoryPABParams protocolPABParams
                                                    fundFactoryPABParams' =
                                                        T.FundFactoryPABParams
                                                            {
                                                                T.ffppFundFactoryVersion  = T.ffppFundFactoryVersion fundFactoryPABParams,
                                                                T.ffppFundValidator_Params = T.ffppFundValidator_Params fundFactoryPABParams,
                                                                T.ffppFundValidator = T.ffppFundValidator fundFactoryPABParams,
                                                                T.ffppFundValidator_Hash = T.ffppFundValidator_Hash fundFactoryPABParams,
                                                                T.ffppFundValidator_Address = T.ffppFundValidator_Address fundFactoryPABParams,
                                                                T.ffppInvestUnitValidator_Params = T.ffppInvestUnitValidator_Params fundFactoryPABParams,
                                                                T.ffppInvestUnitValidator = T.ffppInvestUnitValidator fundFactoryPABParams,
                                                                T.ffppInvestUnitValidator_Hash = T.ffppInvestUnitValidator_Hash fundFactoryPABParams,
                                                                T.ffppInvestUnitValidator_Address = T.ffppInvestUnitValidator_Address fundFactoryPABParams,
                                                                T.ffppFundPABParams              = fundPABParams : T.ffppFundPABParams fundFactoryPABParams
                                                            }
                                                let newProtocolPABParams =
                                                        T.ProtocolPABParams
                                                            { T.pppProtocolFactoryVersion = T.pppProtocolFactoryVersion protocolPABParams,
                                                                T.pppProtocolPolicyID_Params = T.pppProtocolPolicyID_Params protocolPABParams,
                                                              T.pppProtocolPolicyID = T.pppProtocolPolicyID protocolPABParams,
                                                              T.pppProtocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams,
                                                              T.pppProtocolValidator_Params = T.pppProtocolValidator_Params protocolPABParams,
                                                              T.pppProtocolValidator = T.pppProtocolValidator protocolPABParams,
                                                              T.pppProtocolValidator_Hash = T.pppProtocolValidator_Hash protocolPABParams,
                                                              T.pppProtocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams,
                                                              T.pppScriptPolicyID_Params = T.pppScriptPolicyID_Params protocolPABParams,
                                                              T.pppScriptPolicyID = T.pppScriptPolicyID protocolPABParams,
                                                              T.pppScriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams,
                                                              T.pppScriptValidator_Params = T.pppScriptValidator_Params protocolPABParams,
                                                              T.pppScriptValidator = T.pppScriptValidator protocolPABParams,
                                                              T.pppScriptValidator_Hash = T.pppScriptValidator_Hash protocolPABParams,
                                                              T.pppScriptValidator_Address = T.pppScriptValidator_Address protocolPABParams,
                                                              T.pppFundFactoryPABParams = [fundFactoryPABParams']
                                                            }
                                                return newProtocolPABParams
                                    pabMainLoop isAdminMenu (walletNro, walletCount) newProtocolPABParams (Just fundPABParams) pabReturnToMainMenu pabShutdown
                        else do
                            MonadIOClass.liftIO $ P.putStrLn "Invalid input. Can't find FundPAB.json file"
                            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                            pabLoadFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
        else do
            MonadIOClass.liftIO $ P.putStrLn "Invalid input. Can't find Path"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabLoadFundParams isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabFundPrepare :: PABContracts.PABParamsInFundMenu
pabFundPrepare isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Prepare"
    case fundPABParams' of
        (Just fundPABParams) -> do
            adminsNros <- PABHelpers.selectWallets "Fund Admin" [walletNro] walletCount []
            let admins =
                    [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash adminsNro
                      | adminsNro <- adminsNros
                    ]
            fundClassIndex <- MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Fund Class Index" 1
            let
                getToken :: MonadFreerInternal.Eff PABContracts.PABEffects T.InvestUnitToken
                getToken = do
                    MonadIOClass.liftIO $ P.putStrLn "Invest Unit Token"
                    cs <-  MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
                    tn <-  MonadIOClass.liftIO $ CLIHelpers.getTokenName "token" (cs == LedgerApiV2.adaSymbol)
                    amt <-  MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Amount" 10
                    return (cs,tn, amt)

                getTokens :: [T.InvestUnitToken] -> MonadFreerInternal.Eff PABContracts.PABEffects [T.InvestUnitToken]
                getTokens list = do
                    item <- getToken
                    MonadIOClass.liftIO $ P.putStrLn "Do you want to add another Token (y/n - default=n)?"
                    swContinue <- MonadIOClass.liftIO $ CLIHelpers.getBoolWithDefault False
                    if swContinue
                        then do
                            getTokens (list ++ [item])
                        else do
                            return $ list ++ [item]
                getInvestUnit :: MonadFreerInternal.Eff PABContracts.PABEffects T.InvestUnit
                getInvestUnit = do
                    MonadIOClass.liftIO $ P.putStrLn "Create Invest Unit"
                    tokens <- getTokens []
                    let investUnit = T.InvestUnit { T.iuValues = tokens }
                    return investUnit
            ---------------------
            investUnit <- getInvestUnit
            ---------------------
            slot <- PABSimulator.currentSlot >>= MonadIOClass.liftIO . ConcurrentSTM.atomically
            let now = LedgerTimeSlot.slotToEndPOSIXTime DataDefault.def slot
            let nowPlus2Year = now + LedgerApiV2.POSIXTime 63_072_000_000 -- = (2 * 365 * 24 * 60 * 60 * 1000) 63,072,000,000
            MonadIOClass.liftIO $ P.putStrLn $ "Now: " ++ P.show now
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            deadline <-  MonadIOClass.liftIO $ CLIHelpers.getTime "Deadline in Milisecconds" nowPlus2Year 0
            ---------------------
            --commissionsPerYearInBPx1e8 <-  MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Commissions per year in pb x 1e8 (100.000.000 = 0.01% / 1.000.000.000.000 = 100%)" 1000
            commissionsPerYearInBPx1e3 <-  MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Commissions per year in pb x 1e3 (1.000 = 0.01% / 10.000.000 = 100%)" 1000
            ---------------------
            uiPriceADA <-  MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "IU(uFT) ADA Price" 10_000_000
            ---------------------
            let contract =
                    PABContracts.PABFundPrepare
                        T.PABFundPrepareParams
                            {
                                T.pfppProtocolPABParams = protocolPABParams,
                              T.pfppFundPABParams = fundPABParams,
                              T.pfppAdmins = admins,
                              T.pfppFundClassIndex = fundClassIndex,
                              T.pfppBeginAt = now,
                              T.pfppDeadline = deadline,
                              T.pfppClosedAt = Nothing,
                              T.pfppCommissionsPerYearInBPx1e3 = commissionsPerYearInBPx1e3,
                              T.pfppInvestUnit = investUnit,
                              T.pfppInvestUnitPriceADA = uiPriceADA
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

pabFundUpdate :: PABContracts.PABParamsInFundMenu
pabFundUpdate isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Update"
    case fundPABParams' of
        (Just fundPABParams) -> do
            adminsNros <- PABHelpers.selectWallets "Fund Admin" [walletNro] walletCount []
            let admins =
                    [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash adminsNro
                      | adminsNro <- adminsNros
                    ]
                contract = PABContracts.PABFundUpdate T.PABFundUpdateParams{
                    T.pfupProtocolPABParams = protocolPABParams,
                    T.pfupFundPABParams = fundPABParams,
                    T.pfupAdmins = admins
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

pabFundHoldingAdd :: PABContracts.PABParamsInFundMenu
pabFundHoldingAdd isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Add Holding"
    case fundPABParams' of
        (Just fundPABParams) -> do
            let
                contract = PABContracts.PABFundHoldingAdd T.PABFundHoldingAddParams{
                    T.pfhapProtocolPABParams = protocolPABParams,
                    T.pfhapFundPABParams = fundPABParams
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

pabFundHoldingDelete :: PABContracts.PABParamsInFundMenu
pabFundHoldingDelete isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Delete Holding"
    case fundPABParams' of
        (Just fundPABParams) -> do
            !blockchain <- PABSimulator.blockchain
            let
                !address = T.fppFundHoldingValidator_Address fundPABParams
                !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address
                !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
            MonadIOClass.liftIO $ P.putStrLn "Select Holding to Delete"
            selectedUTxO <- PABHelpers.selectUTxOWithDatumAndCS @FundHoldingT.ValidatorDatum fundHoldingPolicyID_CS uTxOuts blockchain
            ----------------
            case selectedUTxO of
                Nothing -> do
                    PABHelpers.waitKeyPress
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                Just (_, txOutRef, _) -> do
                    let
                        contract = PABContracts.PABFundHoldingDelete T.PABFundHoldingDeleteParams{
                            T.pfhdpProtocolPABParams = protocolPABParams,
                            T.pfhdpFundPABParams = fundPABParams,
                            T.pfhdpFundHoldingTxOutRef = txOutRef
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

pabFundReIndexing :: PABContracts.PABParamsInFundMenu
pabFundReIndexing isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Fund Re-Indexing"
    case fundPABParams' of
        (Just fundPABParams) -> do
            let
                getToken :: MonadFreerInternal.Eff PABContracts.PABEffects T.InvestUnitToken
                getToken = do
                    MonadIOClass.liftIO $ P.putStrLn "Invest Unit Token"
                    cs <-  MonadIOClass.liftIO $ CLIHelpers.getCurrencySymbol "6aa8df2a10e8530338279e41a1a40dc73f83e468b710c3b64082f759"
                    tn <-  MonadIOClass.liftIO $ CLIHelpers.getTokenName "token" (cs == LedgerApiV2.adaSymbol)
                    amt <-  MonadIOClass.liftIO $ CLIHelpers.getIntWithDefault "Amount" 10
                    return (cs,tn, amt)

                getTokens :: [T.InvestUnitToken] -> MonadFreerInternal.Eff PABContracts.PABEffects [T.InvestUnitToken]
                getTokens list = do
                    item <- getToken
                    MonadIOClass.liftIO $ P.putStrLn "Do you want to add another Token (y/n - default=n)?"
                    swContinue <- MonadIOClass.liftIO $ CLIHelpers.getBoolWithDefault False
                    if swContinue
                        then do
                            getTokens (list ++ [item])
                        else do
                            return $ list ++ [item]

                getInvestUnit :: MonadFreerInternal.Eff PABContracts.PABEffects T.InvestUnit
                getInvestUnit = do
                    tokens <- getTokens []
                    let investUnit = T.InvestUnit { T.iuValues = tokens }
                    return investUnit
            ----------------
            MonadIOClass.liftIO $ P.putStrLn "Tokens to Remove"
            tokensToRemove <- getInvestUnit
            MonadIOClass.liftIO $ P.putStrLn "Tokens to Add"
            tokensToAdd <- getInvestUnit
            ----------------
            !blockchain <- PABSimulator.blockchain
            let
                !address = T.fppFundHoldingValidator_Address fundPABParams
                !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain address
                !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
            MonadIOClass.liftIO $ P.putStrLn "Select Holding to use for Re-Indexing"
            selectedUTxO <- PABHelpers.selectUTxOWithDatumAndCS @FundHoldingT.ValidatorDatum fundHoldingPolicyID_CS uTxOuts blockchain
            ----------------
            case selectedUTxO of
                Nothing -> do
                    PABHelpers.waitKeyPress
                    pabMainLoop isAdminMenu (walletNro, walletCount) protocolPABParams fundPABParams' pabReturnToMainMenu pabShutdown
                Just (_, txOutRef, _) -> do
                    let contract = PABContracts.PABFundReIndexing T.PABFundReIndexingParams{
                            T.pfripProtocolPABParams = protocolPABParams,
                            T.pfripFundPABParams = fundPABParams,
                            T.pfripTokensToAdd = tokensToAdd,
                            T.pfripTokensToRemove = tokensToRemove,
                            T.pfripFundHoldingTxOutRef = txOutRef
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
