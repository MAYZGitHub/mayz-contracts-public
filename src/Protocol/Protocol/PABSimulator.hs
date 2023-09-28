--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
{-# LANGUAGE TypeApplications #-}
module Protocol.Protocol.PABSimulator where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class       as MonadIOClass (MonadIO (..))
import qualified Data.Maybe                   as DataMaybe
import qualified Ledger
import qualified Ledger.Crypto                as Crypto
import qualified Ledger.Value                 as LedgerValue
import qualified Plutus.PAB.Simulator         as PABSimulator
import           PlutusTx.Prelude             hiding (unless)
import qualified Prelude                      as P
import qualified Text.Read                    as TextRead (readMaybe)
--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2
import qualified Generic.CLIHelpers           as CLIHelpers
import qualified Generic.OffChainHelpers      as OffChainHelpers
import qualified Generic.OnChainHelpers       as OnChainHelpers
import qualified Generic.PABHelpers           as PABHelpers
import qualified Protocol.Constants           as T
import qualified Protocol.PABContracts        as PABContracts
import qualified Protocol.PABHelpers          as PABHelpers
import qualified Protocol.PABTypes            as T
import qualified Protocol.Protocol.Types      as T
import qualified Protocol.Script.PABSimulator as ScriptPABSimulator

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

menuEndPoints :: P.String -> (Integer, Integer) -> T.ProtocolPABParams -> P.IO P.String
menuEndPoints name _ _ = do
    CLIHelpers.printTitle $ "PROTOCOl MENU - " ++ name
    P.putStrLn "31 - Protocol Prepare"
    P.putStrLn "32 - Protocol Update"
    P.putStrLn "--------------------------------"
    P.putStrLn "41 - Add Scripts"
    P.putStrLn "42 - Delete Scripts"
    P.putStrLn "--------------------------------"
    P.putStrLn "0  - Return to Main Menu"
    P.putStrLn "99 - Exit"
    P.putStrLn "--------------------------------"
    P.putStrLn "Enter option:"
    option <- P.getLine
    P.putStrLn "--------------------------------"
    return option

--------------------------------------------------------------------------------2

pabMainLoop :: PABContracts.PABParamsInProtocolMenu
pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    let name = "Protocol: " ++ P.show (T.pppProtocolPolicyID_CS protocolPABParams)
    option <- MonadIOClass.liftIO $ menuEndPoints name (walletNro, walletCount) protocolPABParams
    case option of
        "31" ->
            pabProtocolPrepare (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "32" ->
            pabProtocolUpdate (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "41" ->
            ScriptPABSimulator.pabScriptAddInProtocol  (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabMainLoop pabShutdown
        "42" ->
            ScriptPABSimulator.pabScriptDeleteInProtocol  (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabMainLoop pabShutdown
        "0" ->
            pabReturnToMainMenu (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
        "81" -> do
            PABHelpers.pabBalances (Just walletNro, walletCount) (Just protocolPABParams)
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "82" -> do
            PABHelpers.pabUTxOAtWallet (Just walletNro, walletCount)
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "83" -> do
            PABHelpers.pabUTxOAtScript (Just protocolPABParams)
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "91" -> do
            PABHelpers.pabTimeAndSlot
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown
        "99" ->
            PABHelpers.pabEndSimulation (Just walletNro, walletCount) (Just protocolPABParams) pabShutdown
        _ -> do
            MonadIOClass.liftIO $ P.putStrLn "Invalid option"
            MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown


--------------------------------------------------------------------------------2

pabProtocolPrepare :: PABContracts.PABParamsInProtocolMenu
pabProtocolPrepare (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Prepare"
    ---------------------
    let oracleWallet_PaymentPubKey = OffChainHelpers.seedToPaymentPubKey T.oracleWallet_Seed
    ---------------------
    adminProtocolWalletsNros <- PABHelpers.selectWallets "Protocol Admin" (OffChainHelpers.removeDuplicates [1,2,walletNro]) walletCount []
    let adminProtocolWallets =
            [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
            | walletNro' <- adminProtocolWalletsNros
            ]
    ---------------------
    mayzHolderWalletsNros <- PABHelpers.selectWallets "MAYZ Holders Admin" [3,4] walletCount []
    let mayzHolderWallets =
            [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
            | walletNro' <- mayzHolderWalletsNros
            ]
    ---------------------
    let promptFundClass :: P.IO T.FundClass
        promptFundClass = do
            P.putStrLn "Enter the values for a FundClass:"
            index <- CLIHelpers.getIntWithDefault "Index" 1
            requiredMAYZ <- CLIHelpers.getIntWithDefault "Required MAYZ" 1_000
            maxUI <- CLIHelpers.getIntWithDefault "Max UI" 1_000_000_000
            return (T.FundClass index requiredMAYZ maxUI)
    ---------------------
        promptFundClasses :: P.IO [T.FundClass]
        promptFundClasses = promptFundClasses' []
            where
                promptFundClasses' :: [T.FundClass] -> P.IO [T.FundClass]
                promptFundClasses' acc = do
                    fundClass <- promptFundClass
                    let updatedAcc = fundClass : acc
                    P.putStrLn "FundClass added!"
                    P.putStrLn "Do you want to add one more (y/n - default=n)?"
                    choice <- CLIHelpers.getBoolWithDefault False
                    if choice
                        then promptFundClasses' updatedAcc
                        else return (reverse updatedAcc)
    ---------------------
    fundClasses <- MonadIOClass.liftIO promptFundClasses
    ---------------------
    share_Protocol <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Protocol share (1pb = 0.01% / 10,000pb = 100%)" 3334
    share_MAYZ <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "MAYZ share (1pb = 0.01% / 10,000pb = 100%)" 3333
    share_FundAdmins <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Admins share (1pb = 0.01% / 10,000pb = 100%)" 3333
    ---------------------
    let contract =
            PABContracts.PABProtocolPrepare
                T.PABProtocolPrepareParams
                    { T.ppppProtocolPABParams = protocolPABParams,
                    T.ppppOraclePaymentPubKey = oracleWallet_PaymentPubKey,
                    T.ppppAdmins = adminProtocolWallets,
                    T.ppppFundClasses = fundClasses,
                    T.ppppFundLifeTime = T.mkMinMaxDef 0 0 0,
                    T.ppppRequiredMAYZForSellOffers = 0,
                    T.ppppRequiredMAYZForBuyOrders = 0,
                    T.ppppCommissionFunds = T.mkMinMaxDef 0 0 0,
                    T.ppppCommissionSellOffers = T.mkMinMaxDef 0 0 0,
                    T.ppppCommissionBuyOrders = T.mkMinMaxDef 0 0 0,
                    T.ppppShare_Protocol = share_Protocol,
                    T.ppppShare_MAYZ = share_MAYZ,
                    T.ppppShare_FundAdmins = share_FundAdmins,
                    T.ppppMAYZWallets = mayzHolderWallets
                    }
    contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
    PABHelpers.waitContractAndKeyPress contractInstance
    pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

pabProtocolUpdate :: PABContracts.PABParamsInProtocolMenu
pabProtocolUpdate (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown = do
    MonadIOClass.liftIO $ CLIHelpers.printSubTitle "Protocol Update"
    ---------------------
    blockchain <- PABSimulator.blockchain
    ---------------------
    let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
    ---------------------
    let !uTxOuts = PABHelpers.getUTxOsListInPABSimulator blockchain (T.pppProtocolValidator_Address protocolPABParams)
        !uTxO_With_ProtocolDatum' = [ (txOutRef, txOut,  T.getProtocolDatumType $ OnChainHelpers.fromJust $ PABHelpers.getDatumInPABSimulator @T.ValidatorDatum blockchain (txOutRef, txOut) )
                                                | (txOutRef, txOut) <- uTxOuts,
                                                    LedgerValue.assetClassValueOf (Ledger.txOutValue txOut) protocolID_AC > 0
                                                    && DataMaybe.isJust (PABHelpers.getDatumInPABSimulator @T.ValidatorDatum blockchain (txOutRef, txOut) )
                                            ]
    ---------------------
    case uTxO_With_ProtocolDatum' of
        (uTxO_With_ProtocolDatum:_) -> do
            ---------------------
            let !protocolDatum = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
            ---------------------
            adminProtocolWalletsNros <- PABHelpers.selectWallets "Protocol Admin" (OffChainHelpers.removeDuplicates [1,2,walletNro]) walletCount []
            let adminProtocolWallets =
                    [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
                    | walletNro' <- adminProtocolWalletsNros
                    ]
            ---------------------
            mayzHolderWalletsNros <- PABHelpers.selectWallets "MAYZ Holders Admin" [3,4] walletCount []
            let mayzHolderWallets =
                    [ Ledger.unPaymentPubKeyHash $ PABHelpers.walletPaymentPubKeyHash walletNro'
                    | walletNro' <- mayzHolderWalletsNros
                    ]
            ---------------------
            let promptFundClass :: P.IO T.FundClass
                promptFundClass = do
                    P.putStrLn "Enter the values for a FundClass:"
                    index <- CLIHelpers.getIntWithDefault "Index" 1
                    requiredMAYZ <- CLIHelpers.getIntWithDefault "Required MAYZ" 1_000
                    maxUI <- CLIHelpers.getIntWithDefault "Max UI" 1_000_000_000
                    return (T.FundClass index requiredMAYZ maxUI)
            ---------------------
                promptFundClasses :: P.IO [T.FundClass]
                promptFundClasses = promptFundClasses' []
                    where
                        promptFundClasses' :: [T.FundClass] -> P.IO [T.FundClass]
                        promptFundClasses' acc = do
                            fundClass <- promptFundClass
                            let updatedAcc = fundClass : acc
                            P.putStrLn "FundClass added!"
                            P.putStrLn "Do you want to add one more (y/n - default=n)?"
                            choice <- CLIHelpers.getBoolWithDefault False
                            if choice
                                then promptFundClasses' updatedAcc
                                else return (reverse updatedAcc)
            ---------------------
            fundClasses <- MonadIOClass.liftIO promptFundClasses
            ---------------------
            share_Protocol <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Protocol share (1pb = 0.01% / 10,000pb = 100%)" 3334
            share_MAYZ <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "MAYZ share (1pb = 0.01% / 10,000pb = 100%)" 3333
            share_FundAdmins <- MonadIOClass.liftIO  $ CLIHelpers.getIntWithDefault "Admins share (1pb = 0.01% / 10,000pb = 100%)" 3333
            ---------------------
            -- TODO: leer el resto de los campos que no se actualizan del datum , apra no sobreescebirlos con default
            let
                contract = PABContracts.PABProtocolUpdate T.PABProtocolUpdateParams{
                    T.ppupProtocolPABParams = protocolPABParams,
                    T.ppupOraclePaymentPubKey = T.pdOraclePaymentPubKey protocolDatum,
                    T.ppupAdmins = adminProtocolWallets,
                    T.ppupFundClasses = fundClasses,
                    T.ppupFundLifeTime = T.pdFundLifeTime protocolDatum,
                    T.ppupRequiredMAYZForSellOffers = T.pdRequiredMAYZForSellOffers protocolDatum,
                    T.ppupRequiredMAYZForBuyOrders = T.pdRequiredMAYZForBuyOrders protocolDatum,
                    T.ppupCommissionFunds = T.pdCommissionFunds protocolDatum,
                    T.ppupCommissionSellOffers = T.pdCommissionSellOffers protocolDatum,
                    T.ppupCommissionBuyOrders = T.pdCommissionBuyOrders protocolDatum,
                    T.ppupShare_Protocol = share_Protocol,
                    T.ppupShare_MAYZ = share_MAYZ,
                    T.ppupShare_FundAdmins = share_FundAdmins,
                    T.ppupMAYZWallets = mayzHolderWallets
                }

            contractInstance <- PABSimulator.activateContract (PABHelpers.getWallet walletNro) contract
            PABHelpers.waitContractAndKeyPress contractInstance
            pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

        [] -> do
                MonadIOClass.liftIO $ P.putStrLn "Can't find Protocol Datum"
                MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
                pabMainLoop (walletNro, walletCount) protocolPABParams pabReturnToMainMenu pabShutdown

--------------------------------------------------------------------------------2

