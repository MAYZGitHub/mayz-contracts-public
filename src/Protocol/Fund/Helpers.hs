{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2
module Protocol.Fund.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2
import           PlutusTx.Prelude                    hiding (unless)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Protocol.Fund.Types      as T
import qualified Protocol.Fund.Holding.Types      as FundHoldingT
import qualified Generic.Types as T
import qualified PlutusTx.Ratio as TxRatio
import qualified Generic.OnChainHelpers as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundDatum_With_NormalChanges #-}
mkUpdated_FundDatum_With_NormalChanges :: T.FundDatumType -> [T.WalletPaymentPKH] -> T.FundDatumType
mkUpdated_FundDatum_With_NormalChanges !fundDatum_In !admins =
    T.mkFundDatumType
        (T.fdFundPolicy_CS fundDatum_In)
        (T.fdFundValidator_Hash fundDatum_In)
        (T.fdFundHoldingPolicyID_CS fundDatum_In)
        (T.fdFundHoldingValidator_Hash fundDatum_In)
        (T.fdInvestUnitValidator_Hash fundDatum_In)
        admins
        (T.fdFundClassIndex fundDatum_In)
        (T.fdBeginAt fundDatum_In)
        (T.fdDeadline fundDatum_In)
        (T.fdClosedAt fundDatum_In)
        (T.fdCommissionsPerYearInBPx1e3 fundDatum_In)
        (T.fdHoldingsCount fundDatum_In)
        (T.fdHoldingsIndex fundDatum_In)
        (T.fdHoldingsCreators fundDatum_In)
        (T.fdMinADA fundDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundDatum_With_HoldingAdded #-}
mkUpdated_FundDatum_With_HoldingAdded :: T.FundDatumType -> T.WalletPaymentPKH -> Maybe T.StakeCredentialPubKeyHash -> Integer  -> T.FundDatumType
mkUpdated_FundDatum_With_HoldingAdded !fundDatum_In !creator !creatorStakeCredential !minADA =
    let
        !holdingCreatorsAll = T.fdHoldingsCreators fundDatum_In
        !holdingCreatorsOthers = [ holdingCreator | holdingCreator <- holdingCreatorsAll,  T.hcPaymentPKH holdingCreator /= creator]
        !holdingCreatorOld = find (\holdingCreator -> T.hcPaymentPKH holdingCreator == creator) holdingCreatorsAll
        !holdingCreatorsNew =
            (case holdingCreatorOld :: Maybe T.HoldingCreator of
                Just !hc    -> T.HoldingCreator creator (T.hcStakePKH hc) (T.hcMinADA hc + minADA)
                _           -> T.HoldingCreator creator creatorStakeCredential minADA
                :holdingCreatorsOthers)
    in T.mkFundDatumType
        (T.fdFundPolicy_CS fundDatum_In)
        (T.fdFundValidator_Hash fundDatum_In)
        (T.fdFundHoldingPolicyID_CS fundDatum_In)
        (T.fdFundHoldingValidator_Hash fundDatum_In)
        (T.fdInvestUnitValidator_Hash fundDatum_In)
        (T.fdAdmins fundDatum_In)
        (T.fdFundClassIndex fundDatum_In)
        (T.fdBeginAt fundDatum_In)
        (T.fdDeadline fundDatum_In)
        (T.fdClosedAt fundDatum_In)
        (T.fdCommissionsPerYearInBPx1e3 fundDatum_In)
        (T.fdHoldingsCount fundDatum_In + 1)
        (T.fdHoldingsIndex fundDatum_In + 1)
        holdingCreatorsNew
        (T.fdMinADA fundDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundDatum_With_HoldingDeleted #-}
mkUpdated_FundDatum_With_HoldingDeleted :: T.FundDatumType -> T.FundDatumType
mkUpdated_FundDatum_With_HoldingDeleted !fundDatum_In  =
    T.mkFundDatumType
        (T.fdFundPolicy_CS fundDatum_In)
        (T.fdFundValidator_Hash fundDatum_In)
        (T.fdFundHoldingPolicyID_CS fundDatum_In)
        (T.fdFundHoldingValidator_Hash fundDatum_In)
        (T.fdInvestUnitValidator_Hash fundDatum_In)
        (T.fdAdmins fundDatum_In)
        (T.fdFundClassIndex fundDatum_In)
        (T.fdBeginAt fundDatum_In)
        (T.fdDeadline fundDatum_In)
        (T.fdClosedAt fundDatum_In)
        (T.fdCommissionsPerYearInBPx1e3 fundDatum_In)
        (T.fdHoldingsCount fundDatum_In - 1)
        (T.fdHoldingsIndex fundDatum_In)
        (T.fdHoldingsCreators fundDatum_In)
        (T.fdMinADA fundDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHoldingDatum_With_Deposit #-}
mkUpdated_FundHoldingDatum_With_Deposit :: FundHoldingT.FundHoldingDatumType -> Integer  -> Integer  -> Integer -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHoldingDatum_With_Deposit !fundHoldingDatum_In !mintingFT !deliverFT !commissionsPayed !commissions_RatePerMonth_Numerator1e6 =
        FundHoldingT.mkFundHoldingDatumType
        (FundHoldingT.hdFundHolding_Index fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_FT_Minted_Accumulated fundHoldingDatum_In + mintingFT )
        (FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In + mintingFT)
        (FundHoldingT.hdSubtotal_FT_Circulation fundHoldingDatum_In + deliverFT)
        (FundHoldingT.hdSubtotal_FT_ForComission fundHoldingDatum_In + commissionsPayed)
        (FundHoldingT.hdSubtotal_FT_ForComission_Acumulated fundHoldingDatum_In + commissionsPayed)
        (FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 fundHoldingDatum_In + commissions_RatePerMonth_Numerator1e6)
        (FundHoldingT.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In)
        (FundHoldingT.hdMinADA fundHoldingDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHoldingDatum_With_Withdraw #-}
mkUpdated_FundHoldingDatum_With_Withdraw :: FundHoldingT.FundHoldingDatumType ->Integer  -> Integer  -> Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHoldingDatum_With_Withdraw !fundHoldingDatum_In !withdraw !commissionsForUserFTToGetBack !commissions_RatePerMonth_Numerator1e6 =
    FundHoldingT.mkFundHoldingDatumType
        (FundHoldingT.hdFundHolding_Index fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_FT_Minted_Accumulated fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In - withdraw - commissionsForUserFTToGetBack)
        (FundHoldingT.hdSubtotal_FT_Circulation fundHoldingDatum_In - withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission fundHoldingDatum_In - commissionsForUserFTToGetBack)
        (FundHoldingT.hdSubtotal_FT_ForComission_Acumulated fundHoldingDatum_In - commissionsForUserFTToGetBack)
        (FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 fundHoldingDatum_In - commissions_RatePerMonth_Numerator1e6)
        (FundHoldingT.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In)
        (FundHoldingT.hdMinADA fundHoldingDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHoldingDatum_With_Collect_Protocol_Commissions #-}
mkUpdated_FundHoldingDatum_With_Collect_Protocol_Commissions :: FundHoldingT.FundHoldingDatumType ->Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHoldingDatum_With_Collect_Protocol_Commissions !fundHoldingDatum_In !withdraw  =
    FundHoldingT.mkFundHoldingDatumType
        (FundHoldingT.hdFundHolding_Index fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_FT_Minted_Accumulated fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Circulation fundHoldingDatum_In + withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission fundHoldingDatum_In - withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission_Acumulated fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In + withdraw)
        (FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In)
        (FundHoldingT.hdMinADA fundHoldingDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHoldingDatum_With_Collect_MAYZ_Commissions #-}
mkUpdated_FundHoldingDatum_With_Collect_MAYZ_Commissions :: FundHoldingT.FundHoldingDatumType ->Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHoldingDatum_With_Collect_MAYZ_Commissions !fundHoldingDatum_In !withdraw  =
    FundHoldingT.mkFundHoldingDatumType
        (FundHoldingT.hdFundHolding_Index fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_FT_Minted_Accumulated fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Circulation fundHoldingDatum_In + withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission fundHoldingDatum_In - withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission_Acumulated fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In + withdraw)
        (FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In)
        (FundHoldingT.hdMinADA fundHoldingDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHoldingDatum_With_Collect_FundAdmins_Commissions #-}
mkUpdated_FundHoldingDatum_With_Collect_FundAdmins_Commissions :: FundHoldingT.FundHoldingDatumType -> Integer  -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHoldingDatum_With_Collect_FundAdmins_Commissions !fundHoldingDatum_In !withdraw  =
    FundHoldingT.mkFundHoldingDatumType
        (FundHoldingT.hdFundHolding_Index fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_FT_Minted_Accumulated fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Minted fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_FT_Circulation fundHoldingDatum_In + withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission fundHoldingDatum_In - withdraw)
        (FundHoldingT.hdSubtotal_FT_ForComission_Acumulated fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_Protocol fundHoldingDatum_In )
        (FundHoldingT.hdSubtotal_Collected_Commissions_MAYZ fundHoldingDatum_In)
        (FundHoldingT.hdSubtotal_Collected_Commissions_FundAdmins fundHoldingDatum_In + withdraw)
        (FundHoldingT.hdMinADA fundHoldingDatum_In)

--------------------------------------------------------------------------------2

{-# INLINEABLE getRemainingMonths #-}
getRemainingMonths :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer
getRemainingMonths deadline date =
    let
        !msPerMonth = 2_592_000_000 :: Integer -- 1000 * 60 * 60 * 24 * 30
        !msRemaining =  LedgerApiV2.getPOSIXTime $ deadline - date
        !monthsRemaining = msRemaining `divide` msPerMonth
    in monthsRemaining

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateDepositComissionsUsingMonths #-}
calculateDepositComissionsUsingMonths :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> (Integer, Integer, Integer)
calculateDepositComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date deposit = (userFT, commissionsFT, commissions_RatePerMonth_Numerator1e6)
  where
    -- las comisiones son por en realidad por año y en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
    -- la formula de comisiones es = (1 - comisiones por periodo en porcentaje del 0 al 1) ^ (periodos restantes + 1)
    -- el periodo aqui lo voy a calcular en meses
    -- eso significa que al valor de commissionsPerYearInBPx1e3 tengo que dividirlo por 
    -- 10e2 para pasarlo a bp
    -- 100 para pasarlo a porcentaje normal del 1 al 100
    -- 100 para pasarlo a porcentaje del 0 al 1
    -- 12 para pasarlo a meses
    -- NO VA MAS => defino den = 1e8 * 100 * 100 * 12 = 100_000_000 * 100 * 100 * 12 = 12_000_000_000_000
    -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120_000_000
    -- commissionesPerMonthPct0to1 = TxRatio.unsafeRatio commissionsPerYearInBPx1e3 den
    -- pero en lugar de calcular el rational y leugo volver a separarlo en num y den para llamar a la potencia
    -- voy a calcular primero el 1 - commissionesPerMonthPct0to1 de esta forma:
    -- commissionesToUse = TxRatio.unsafeRatio (den - commissionsPerYearInBPx1e3) den
    -- pero este Rational tampoco hace falta crearlo, puedo usar directamente esos num y den para llamar a la potencia
    -- commissionsAcumulated = OnChainHelpers.powRational (den - commissionsPerYearInBPx1e3) den (daysRemaining + 1)
    ------------------
    !monthsRemaining = getRemainingMonths deadline date
    ------------------
    -- voy a probar con 1e3
    -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120000000
    !den = 120_000_000 
    !commissionsAcumulated = OnChainHelpers.powRational (den - commissionsPerYearInBPx1e3) den (monthsRemaining + 1)
    -- !commissionsAcumulated' = OnChainHelpers.setAndLoosePrecision1e6 commissionsAcumulated
    ------------------
    !userFT = TxRatio.truncate (commissionsAcumulated * TxRatio.fromInteger deposit)
    !commissionsFT = deposit - userFT
    !commissionsRatePerMonth 
        | monthsRemaining == 0 = TxRatio.fromInteger 0
        | otherwise =  TxRatio.unsafeRatio commissionsFT monthsRemaining    
    !commissions_RatePerMonth_Numerator1e6 = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissionsRatePerMonth


--------------------------------------------------------------------------------2

{-# INLINEABLE calculateWithdrawComissionsUsingMonths #-}
calculateWithdrawComissionsUsingMonths :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> (Integer, Integer, Integer)
calculateWithdrawComissionsUsingMonths commissionsPerYearInBPx1e3 deadline date withdraw = (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_RatePerMonth_Numerator1e6)
    where
    -- las comisiones son por en realidad por año y en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
    -- la formula de comisiones es = (1 - comisiones por periodo en porcentaje del 0 al 1) ^ (periodos restantes + 1)
    -- el periodo aqui lo voy a calcular en meses
    -- eso significa que al valor de commissionsPerYearInBPx1e3 tengo que dividirlo por 
    -- 10e2 para pasarlo a bp
    -- 100 para pasarlo a porcentaje normal del 1 al 100
    -- 100 para pasarlo a porcentaje del 0 al 1
    -- 12 para pasarlo a meses
    -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120_000_000
    -- commissionesPerMonthPct0to1 = TxRatio.unsafeRatio commissionsPerYearInBPx1e3 den
    -- pero en lugar de calcular el rational y leugo volver a separarlo en num y den para llamar a la potencia
    -- voy a calcular primero el 1 - commissionesPerMonthPct0to1 de esta forma:
    -- commissionesToUse = TxRatio.unsafeRatio (den - commissionsPerYearInBPx1e3) den
    -- pero este Rational tampoco hace falta crearlo, puedo usar directamente esos num y den para llamar a la potencia
    -- commissionsAcumulated = OnChainHelpers.powRational (den - commissionsPerYearInBPx1e3) den (daysRemaining + 1)
    ------------------
    !monthsRemaining = getRemainingMonths deadline date
    ------------------
    !den = 120_000_000
    !commissionsAcumulatedNotIncludingThisPeriod = OnChainHelpers.powRational (den - commissionsPerYearInBPx1e3) den monthsRemaining
    ------------------
    !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (commissionsAcumulatedNotIncludingThisPeriod * TxRatio.fromInteger withdraw)
    !commissionsForUserFTToGetBack = withdraw - userFT'forCalculationsOfCommissionsToGetBack
    !withdrawPlusCommissionsGetBack = withdraw + commissionsForUserFTToGetBack
    !commissionsRatePerMonth 
        | monthsRemaining == 0 = TxRatio.fromInteger 0
        | otherwise =  TxRatio.unsafeRatio commissionsForUserFTToGetBack monthsRemaining
    !commissions_RatePerMonth_Numerator1e6 = OnChainHelpers.setAndLoosePrecisionGetOnlyNum commissionsRatePerMonth 6

--------------------------------------------------------------------------------2

{-# INLINEABLE getCommissionsAvailable #-}
getCommissionsAvailable :: LedgerApiV2.POSIXTime ->FundHoldingT.FundHoldingDatumType ->  Integer -> Integer -> LedgerApiV2.POSIXTime -> Integer
getCommissionsAvailable deadline fundHoldingDatum_In share taken date =
    let
        !monthsRemainingRational = getRemainingMonths deadline date
        !totalCommisionsAcum = FundHoldingT.hdSubtotal_FT_ForComission_Acumulated fundHoldingDatum_In
        !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubtotal_Commissions_RatePerMonth_Numerator1e6 fundHoldingDatum_In) 1_000_000
        !commisionsReady = TxRatio.fromInteger totalCommisionsAcum - (TxRatio.fromInteger monthsRemainingRational * rate)
        !sharePct = TxRatio.unsafeRatio share 10_000
        !commisionsReady_share = commisionsReady * sharePct
    in TxRatio.truncate commisionsReady_share - taken

--------------------------------------------------------------------------------2
