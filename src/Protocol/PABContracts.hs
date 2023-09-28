{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}

--------------------------------------------------------------------------------2
module Protocol.PABContracts where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Control.Monad.Freer.Internal        as MonadFreerInternal (Eff)
import qualified Data.Aeson                          as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Data.Text                           as DataText
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Playground.Contract                 as PlaygroundContract
import qualified Plutus.Contract                     as PlutusContract
import qualified Plutus.PAB.Effects.Contract.Builtin as PABEffectsContractBuiltin
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                             as P
import qualified Prettyprinter

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.PABHelpers                  as PABHelpers
import qualified Protocol.Fund.OffChain              as OffChain
import qualified Protocol.Others.OffChain            as OffChain
import qualified Protocol.PABTypes                   as T
import qualified Protocol.Protocol.OffChain          as OffChain
import qualified Protocol.Script.OffChain            as OffChain

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

type ProtocolSchema =
        PlutusContract.Endpoint "endPointSplitUtxO" T.PABSplitUtxOParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointMintFT" T.PABMintFTParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointMintNFT" T.PABMintNFTParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointMintFundTokens" T.PABMintFundTokensParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointProtocolPrepare" T.PABProtocolPrepareParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointProtocolUpdate" T.PABProtocolUpdateParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointProtocolScriptAdd" T.PABProtocolScriptAddParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointProtocolScriptDelete" T.PABProtocolScriptDeleteParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointFundPrepare" T.PABFundPrepareParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundUpdate" T.PABFundUpdateParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundScriptAdd" T.PABFundScriptAddParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundScriptDelete" T.PABFundScriptDeleteParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointFundHoldingAdd" T.PABFundHoldingAddParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundHoldingDelete" T.PABFundHoldingDeleteParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointFundDeposit" T.PABFundDepositParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundWithdraw" T.PABFundWithdrawParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointFundReIndexing" T.PABFundReIndexingParams

        PlutusContract..\/ PlutusContract.Endpoint "endPointFundCollect_Protocol_Commissions" T.PABFundCollect_Protocol_CommissionsParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundCollect_MAYZ_Commissions" T.PABFundCollect_MAYZ_CommissionsParams
        PlutusContract..\/ PlutusContract.Endpoint "endPointFundCollect_FundAdmins_Commissions" T.PABFundCollect_FundAdmins_CommissionsParams

protocolEndPoints :: PlutusContract.Contract () ProtocolSchema DataText.Text ()
protocolEndPoints =
    PlutusContract.awaitPromise
        (
            endPointSplitUtxO'
            `PlutusContract.select` endPointMintFT'
            `PlutusContract.select` endPointMintNFT'

            `PlutusContract.select` endPointProtocolPrepare'
            `PlutusContract.select` endPointProtocolUpdate'
            `PlutusContract.select` endPointProtocolScriptAdd'
            `PlutusContract.select` endPointProtocolScriptDelete'

            `PlutusContract.select` endPointFundPrepare'
            `PlutusContract.select` endPointFundUpdate'
            `PlutusContract.select` endPointFundScriptAdd'
            `PlutusContract.select` endPointFundScriptDelete'

            `PlutusContract.select` endPointFundHoldingAdd'
            `PlutusContract.select` endPointFundHoldingDelete'

            `PlutusContract.select` endPointFundDeposit'
            `PlutusContract.select` endPointFundWithdraw'

            `PlutusContract.select` endPointFundReIndexing'

            `PlutusContract.select` endPointFundCollect_Protocol_Commissions'
            `PlutusContract.select` endPointFundCollect_MAYZ_Commissions'
            `PlutusContract.select` endPointFundCollect_FundAdmins_Commissions'
        )
        >> protocolEndPoints
    where

        endPointSplitUtxO' = PlutusContract.endpoint @"endPointSplitUtxO" OffChain.endPointSplitUtxO
        endPointMintFT' = PlutusContract.endpoint @"endPointMintFT" OffChain.endPointMintFT
        endPointMintNFT' = PlutusContract.endpoint @"endPointMintNFT" OffChain.endPointMintNFT

        endPointProtocolPrepare' = PlutusContract.endpoint @"endPointProtocolPrepare" OffChain.endPointProtocolPrepare
        endPointProtocolUpdate' = PlutusContract.endpoint @"endPointProtocolUpdate" OffChain.endPointProtocolUpdate

        endPointProtocolScriptAdd' = PlutusContract.endpoint @"endPointProtocolScriptAdd" OffChain.endPointProtocolScriptAdd
        endPointProtocolScriptDelete' = PlutusContract.endpoint @"endPointProtocolScriptDelete" OffChain.endPointProtocolScriptDelete

        endPointFundPrepare' = PlutusContract.endpoint @"endPointFundPrepare" OffChain.endPointFundPrepare
        endPointFundUpdate' = PlutusContract.endpoint @"endPointFundUpdate" OffChain.endPointFundUpdate
        endPointFundScriptAdd' = PlutusContract.endpoint @"endPointFundScriptAdd" OffChain.endPointFundScriptAdd
        endPointFundScriptDelete' = PlutusContract.endpoint @"endPointFundScriptDelete" OffChain.endPointFundScriptDelete

        endPointFundHoldingAdd' = PlutusContract.endpoint @"endPointFundHoldingAdd" OffChain.endPointFundHoldingAdd
        endPointFundHoldingDelete' = PlutusContract.endpoint @"endPointFundHoldingDelete" OffChain.endPointFundHoldingDelete

        endPointFundDeposit' = PlutusContract.endpoint @"endPointFundDeposit" OffChain.endPointFundDeposit
        endPointFundWithdraw' = PlutusContract.endpoint @"endPointFundWithdraw" OffChain.endPointFundWithdraw

        endPointFundReIndexing' = PlutusContract.endpoint @"endPointFundReIndexing" OffChain.endPointFundReIndexing

        endPointFundCollect_Protocol_Commissions' = PlutusContract.endpoint @"endPointFundCollect_Protocol_Commissions" OffChain.endPointFundCollect_Protocol_Commissions
        endPointFundCollect_MAYZ_Commissions' = PlutusContract.endpoint @"endPointFundCollect_MAYZ_Commissions" OffChain.endPointFundCollect_MAYZ_Commissions
        endPointFundCollect_FundAdmins_Commissions' = PlutusContract.endpoint @"endPointFundCollect_FundAdmins_Commissions" OffChain.endPointFundCollect_FundAdmins_Commissions

PlaygroundContract.mkSchemaDefinitions ''ProtocolSchema

--------------------------------------------------------------------------------2

data PABContracts
    = PABSplitUtxO T.PABSplitUtxOParams
    | PABMintFT T.PABMintFTParams
    | PABMintNFT T.PABMintNFTParams
    | PABMintFundTokens T.PABMintFundTokensParams
    | PABProtocolPrepare T.PABProtocolPrepareParams
    | PABProtocolUpdate T.PABProtocolUpdateParams
    | PABProtocolScriptAdd T.PABProtocolScriptAddParams
    | PABProtocolScriptDelete T.PABProtocolScriptDeleteParams
    | PABFundPrepare T.PABFundPrepareParams
    | PABFundUpdate T.PABFundUpdateParams
    | PABFundScriptAdd T.PABFundScriptAddParams
    | PABFundScriptDelete T.PABFundScriptDeleteParams
    | PABFundHoldingAdd T.PABFundHoldingAddParams
    | PABFundHoldingDelete T.PABFundHoldingDeleteParams
    | PABFundDeposit T.PABFundDepositParams
    | PABFundWithdraw T.PABFundWithdrawParams
    | PABFundReIndexing T.PABFundReIndexingParams
    | PABFundCollect_Protocol_Commissions T.PABFundCollect_Protocol_CommissionsParams
    | PABFundCollect_MAYZ_Commissions T.PABFundCollect_MAYZ_CommissionsParams
    | PABFundCollect_FundAdmins_Commissions T.PABFundCollect_FundAdmins_CommissionsParams
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Prettyprinter.Pretty PABContracts where
    pretty = Prettyprinter.viaShow

instance PABEffectsContractBuiltin.HasDefinitions PABContracts where
    getDefinitions =
        [
          PABSplitUtxO T.examplePABSplitUtxOParams,
          PABMintFT T.examplePABMintFTParams,
          PABMintNFT T.examplePABMintNFTParams,

          PABMintFundTokens T.examplePABMintFundTokensParams,

          PABProtocolPrepare T.examplePABProtocolPrepareParams,
          PABProtocolUpdate T.examplePABProtocolUpdateParams,
          PABProtocolScriptAdd T.examplePABProtocolScriptAddParams,
          PABProtocolScriptDelete T.examplePABProtocolScriptDeleteParams,

          PABFundPrepare T.examplePABFundPrepareParams,
          PABFundUpdate T.examplePABFundUpdateParams,
          PABFundScriptAdd T.examplePABFundScriptAddParams,
          PABFundScriptDelete T.examplePABFundScriptDeleteParams,

          PABFundHoldingAdd T.examplePABFundHoldingAddParams,
          PABFundHoldingDelete T.examplePABFundHoldingDeleteParams,

          PABFundDeposit T.examplePABFundDepositParams,
          PABFundWithdraw T.examplePABFundWithdrawParams,

          PABFundReIndexing T.examplePABFundReIndexingParams,

          PABFundCollect_Protocol_Commissions T.examplePABFundCollect_Protocol_CommissionsParams,
          PABFundCollect_MAYZ_Commissions T.examplePABFundCollect_MAYZ_CommissionsParams,
          PABFundCollect_FundAdmins_Commissions T.examplePABFundCollect_FundAdmins_CommissionsParams


        ]

    getContract (PABSplitUtxO mcParams)                        = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointSplitUtxO @() @PABEffectsContractBuiltin.Empty mcParams
    getContract (PABMintFT mcParams)                           = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointMintFT @() @PABEffectsContractBuiltin.Empty mcParams
    getContract (PABMintNFT mcParams)                          = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointMintNFT @() @PABEffectsContractBuiltin.Empty mcParams

    getContract (PABMintFundTokens mcParams)                   = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointMintFundTokens @() @PABEffectsContractBuiltin.Empty mcParams

    getContract (PABProtocolPrepare params)                    = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointProtocolPrepare @() @PABEffectsContractBuiltin.Empty params
    getContract (PABProtocolUpdate params)                     = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointProtocolUpdate @() @PABEffectsContractBuiltin.Empty params
    getContract (PABProtocolScriptAdd params)                  = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointProtocolScriptAdd @() @PABEffectsContractBuiltin.Empty params
    getContract (PABProtocolScriptDelete params)               = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointProtocolScriptDelete @() @PABEffectsContractBuiltin.Empty params

    getContract (PABFundPrepare params)                        = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundPrepare @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundUpdate params)                         = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundUpdate @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundScriptAdd params)                      = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundScriptAdd @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundScriptDelete params)                   = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundScriptDelete @() @PABEffectsContractBuiltin.Empty params

    getContract (PABFundHoldingAdd params)                     = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundHoldingAdd @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundHoldingDelete params)                  = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundHoldingDelete @() @PABEffectsContractBuiltin.Empty params

    getContract (PABFundDeposit params)                        = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundDeposit @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundWithdraw params)                       = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundWithdraw @() @PABEffectsContractBuiltin.Empty params

    getContract (PABFundReIndexing params)                     = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundReIndexing @() @PABEffectsContractBuiltin.Empty params

    getContract (PABFundCollect_Protocol_Commissions params)   = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundCollect_Protocol_Commissions @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundCollect_MAYZ_Commissions params)       = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundCollect_MAYZ_Commissions @() @PABEffectsContractBuiltin.Empty params
    getContract (PABFundCollect_FundAdmins_Commissions params) = PABEffectsContractBuiltin.SomeBuiltin $ OffChain.endPointFundCollect_FundAdmins_Commissions @() @PABEffectsContractBuiltin.Empty params

    getSchema = const $ PABEffectsContractBuiltin.endpointsToSchemas @PABEffectsContractBuiltin.Empty

--------------------------------------------------------------------------------2

type PABEffects = PABHelpers.PABEffects PABContracts

type PABParamsInMainMenu = (Maybe Integer, Integer) -> Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects ()
type PABParamsInMainMenu' a = (Maybe Integer, Integer) -> Maybe T.ProtocolPABParams -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects a
type PABParamsInOthersMenu = (Maybe Integer, Integer) -> Maybe T.ProtocolPABParams -> PABParamsInMainMenu -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects ()

type PABParamsInProtocolMenu = (Integer, Integer) -> T.ProtocolPABParams -> PABParamsInMainMenu -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects ()
type PABParamsInProtocolScriptMenu = (Integer, Integer) -> T.ProtocolPABParams -> PABParamsInMainMenu -> PABParamsInProtocolMenu -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects ()

type PABParamsInFundMenu = Bool -> (Integer, Integer) -> T.ProtocolPABParams -> Maybe T.FundPABParams -> PABParamsInMainMenu -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects ()
type PABParamsInFundMenu' a = Bool -> (Integer, Integer) -> T.ProtocolPABParams -> Maybe T.FundPABParams -> PABParamsInMainMenu -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects a
type PABParamsInFundScriptMenu = (Integer, Integer) -> T.ProtocolPABParams -> Maybe T.FundPABParams -> PABParamsInMainMenu -> PABParamsInFundMenu -> MonadFreerInternal.Eff PABEffects () -> MonadFreerInternal.Eff PABEffects ()

--------------------------------------------------------------------------------2
