module SharedLogic.RideTax
  ( resolveTaxRates,
    resolveCancellationTax,
    resolveTaxBreakups,
    TaxableAmounts (..),
    noTaxableAmounts,
    fareTaxesOf,
    EarningsTaxLine (..),
    earningsTaxLines,
    airportEntryFeeGross,
    airportEntryFeeTaxed,
    parkingWithAirportEntryFee,
    cityRideTax,
    bookingTax,
    gstBreakupOf,
    splitCommissionInclusive,
    gstBreakupFromTax,
  )
where

import BecknV2.OnDemand.FareBreakup.Core (RideTaxRates (..))
import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import BecknV2.OnDemand.FareBreakup.Tax (TaxBreakup (..))
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified BecknV2.OnDemand.PaymentCharge as PaymentCharge
import Control.Applicative ((<|>))
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.CancellationConsequenceMatrix as DCCM
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney (..))

resolveTaxRates ::
  ( HasField "rideTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "tollTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "parkingTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "commissionTaxConfig" fp (Maybe Tax.TaxConfig)
  ) =>
  DTC.TransporterConfig ->
  Maybe fp ->
  Maybe PaymentCharge.PaymentChargeDetails ->
  Tax.RideTax
resolveTaxRates tc mbFarePolicy mbPaymentCharge =
  Tax.RideTax
    { regime = taxRegime,
      riderPayable =
        RideTaxRates
          { rideFare = cityRate rideTax,
            toll = cityRate (mbFarePolicy >>= (.tollTaxConfig)),
            parking = cityRate (mbFarePolicy >>= (.parkingTaxConfig)),
            airportEntryFee = cityRate (tc.taxConfig.airportEntryFeeTaxConfig <|> gstOnly (gstTax (fromMaybe tc.taxConfig.rideGst tc.taxConfig.airportEntryFeeGst))),
            cancellationFee = Nothing,
            platformFee = Nothing,
            paymentCharge = paymentChargeRateBorneBy [PAYMENT_CUSTOMER]
          },
      driverDeductible =
        Tax.DriverDeductible
          { commission = mbFarePolicy >>= (.commissionTaxConfig),
            cancellationCommission = Nothing,
            paymentCharge = paymentChargeRateBorneBy [PAYMENT_CUSTOMER, PAYMENT_DRIVER]
          },
      platformDeductible = Tax.PlatformDeductible {paymentCharge = paymentChargeRateBorneBy [PAYMENT_PLATFORM]}
    }
  where
    taxRegime = fromMaybe (if isJust (gstTax tc.taxConfig.rideGst) then Tax.GST else Tax.NONE) tc.taxConfig.taxRegime
    cityRate t = if taxRegime == Tax.NONE then Nothing else t
    gstOnly t = if taxRegime == Tax.GST then t else Nothing
    rideTax = (mbFarePolicy >>= (.rideTaxConfig)) <|> gstOnly (gstTax tc.taxConfig.rideGst)
    paymentChargeRateBorneBy bearers = mfilter ((`elem` bearers) . (.bearer)) mbPaymentCharge >>= (.chargeTax)

resolveCancellationTax :: DCCM.CancellationConsequenceMatrix -> Tax.RideTax -> Tax.RideTax
resolveCancellationTax row rideTax =
  Tax.RideTax
    { regime = rideTax.regime,
      riderPayable = RideTaxRates {rideFare = rates.rideFare, toll = rates.toll, parking = rates.parking, airportEntryFee = rates.airportEntryFee, cancellationFee = commissionAndTax >>= \c -> (\pct -> Tax.TaxConfig {tax = asPct pct, cgst = asPct <$> c.cgstPercentage, sgst = asPct <$> c.sgstPercentage, igst = asPct <$> c.igstPercentage}) <$> c.taxPercentage, platformFee = rates.platformFee, paymentCharge = rates.paymentCharge},
      driverDeductible = currentDriverDeductible {Tax.cancellationCommission = commissionAndTax >>= (.commissionTax)},
      platformDeductible = rideTax.platformDeductible
    }
  where
    rates = rideTax.riderPayable
    currentDriverDeductible = rideTax.driverDeductible
    commissionAndTax = row.customerCommissionAndTax
    asPct = fromRational . (.getHighPrecMoney)

cityRideTax :: DTC.TransporterConfig -> Tax.RideTax
cityRideTax tc = resolveTaxRates tc (Nothing :: Maybe DFP.FullFarePolicy) Nothing

data TaxableAmounts = TaxableAmounts
  { commission :: Maybe HighPrecMoney,
    cancellationFee :: Maybe Tax.TaxableAmount,
    cancellationCommission :: Maybe HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney
  }

noTaxableAmounts :: TaxableAmounts
noTaxableAmounts = TaxableAmounts {commission = Nothing, cancellationFee = Nothing, cancellationCommission = Nothing, paymentCharge = Nothing}

resolveTaxBreakups :: Currency -> Maybe Tax.RideTax -> Maybe DFParams.FareParameters -> TaxableAmounts -> Tax.TaxBreakups
resolveTaxBreakups currency mbRideTax mbFareParams amounts =
  Tax.TaxBreakups
    { riderPayable =
        RideTaxRates
          { rideFare = stored.rideFare,
            toll = stored.toll,
            parking = stored.parking,
            airportEntryFee = stored.airportEntryFee,
            cancellationFee = maybe stored.cancellationFee cancellationFeeTax amounts.cancellationFee,
            platformFee = stored.platformFee,
            paymentCharge = stored.paymentCharge
          },
      driverDeductible =
        Tax.DriverDeductible
          { commission = commissionTax <$> amounts.commission,
            cancellationCommission = amounts.cancellationCommission >>= cancellationCommissionTax,
            paymentCharge = paymentChargeTax (.driverDeductible.paymentCharge)
          },
      platformDeductible = Tax.PlatformDeductible {paymentCharge = paymentChargeTax (.platformDeductible.paymentCharge)}
    }
  where
    stored = maybe (pure Nothing) fareTaxesOf mbFareParams
    commissionTax gross = fromMaybe TaxBreakup {total = 0, cgst = Nothing, sgst = Nothing, igst = Nothing} (mbRideTax >>= \rt -> Tax.taxOn currency rt.regime rt.driverDeductible.commission (Tax.Inclusive gross))
    cancellationFeeTax taxable =
      mbRideTax >>= \rt -> Tax.taxOn currency rt.regime rt.riderPayable.cancellationFee taxable
    cancellationCommissionTax gross = mbRideTax >>= \rt -> Tax.taxOn currency rt.regime rt.driverDeductible.cancellationCommission (Tax.Inclusive gross)
    paymentChargeTax rateOf = do
      gross <- amounts.paymentCharge
      rate <- mbRideTax >>= rateOf
      Tax.taxOn currency (Tax.chargeRegime rate) (Just rate) (Tax.Inclusive gross)

fareTaxesOf :: DFParams.FareParameters -> Tax.FareTaxes
fareTaxesOf fp =
  RideTaxRates
    { rideFare = Tax.mkTaxBreakup fp.govtCharges fp.rideTaxCgst fp.rideTaxSgst fp.rideTaxIgst,
      toll = Tax.mkTaxBreakup fp.tollFareTax fp.tollTaxCgst fp.tollTaxSgst fp.tollTaxIgst,
      parking = Tax.mkTaxBreakup fp.parkingChargeTax fp.parkingTaxCgst fp.parkingTaxSgst fp.parkingTaxIgst,
      airportEntryFee = Tax.mkTaxBreakup fp.airportEntryFeeTax fp.airportEntryFeeTaxCgst fp.airportEntryFeeTaxSgst fp.airportEntryFeeTaxIgst,
      cancellationFee = Tax.mkTaxBreakup fp.customerCancellationDuesTax fp.cancellationTaxCgst fp.cancellationTaxSgst fp.cancellationTaxIgst,
      platformFee = Nothing,
      paymentCharge = Tax.mkTaxBreakup fp.paymentProcessingFeeTax fp.paymentChargeTaxCgst fp.paymentChargeTaxSgst fp.paymentChargeTaxIgst
    }

data EarningsTaxLine = EarningsTaxLine
  { title :: Text,
    amount :: HighPrecMoney,
    splits :: [(Text, HighPrecMoney)]
  }

earningsTaxLines :: DFParams.FareParameters -> [EarningsTaxLine]
earningsTaxLines fp =
  catMaybes
    [ line taxes.rideFare FB.taxTags.rideFare,
      line taxes.toll FB.taxTags.toll,
      line taxes.parking FB.taxTags.parking,
      line taxes.cancellationFee FB.taxTags.cancellationFee
    ]
  where
    taxes = fareTaxesOf fp
    line mbBreakup tags = do
      breakup <- mfilter ((> 0) . (.total)) mbBreakup
      pure
        EarningsTaxLine
          { title = FB.tagTitle tags.tax,
            amount = breakup.total,
            splits = [(FB.tagTitle tag, splitAmount) | (tag, Just splitAmount) <- [(tags.cgst, breakup.cgst), (tags.sgst, breakup.sgst), (tags.igst, breakup.igst)], splitAmount > 0]
          }

airportEntryFeeTaxed :: Currency -> Tax.RideTax -> HighPrecMoney -> (HighPrecMoney, Maybe TaxBreakup)
airportEntryFeeTaxed currency rideTax configuredFee = (configuredFee - Tax.taxTotal tax, tax)
  where
    tax = Tax.taxOn currency rideTax.regime rideTax.riderPayable.airportEntryFee (Tax.Inclusive configuredFee)

airportEntryFeeGross :: DFParams.FareParameters -> Maybe HighPrecMoney
airportEntryFeeGross fp = (+ fromMaybe 0 fp.airportEntryFeeTax) <$> fp.airportEntryFee

parkingWithAirportEntryFee :: DFParams.FareParameters -> Maybe HighPrecMoney
parkingWithAirportEntryFee fp = case (fp.parkingCharge, airportEntryFeeGross fp) of
  (Nothing, Nothing) -> Nothing
  (mbParking, mbAirport) -> Just (fromMaybe 0 mbParking + fromMaybe 0 mbAirport)

gstTax :: DTC.GstBreakup -> Maybe Tax.TaxConfig
gstTax gst =
  if totalPct > 0
    then Just Tax.TaxConfig {tax = totalPct, cgst = asPct gst.cgstPercentage, sgst = asPct gst.sgstPercentage, igst = asPct gst.igstPercentage}
    else Nothing
  where
    asPct = fmap (\fraction -> fromRational (fraction.getHighPrecMoney * 100))
    totalPct = sum (mapMaybe asPct [gst.cgstPercentage, gst.sgstPercentage, gst.igstPercentage])

bookingTax :: DRB.Booking -> DTC.TransporterConfig -> Tax.RideTax
bookingTax booking tc = fromMaybe (cityRideTax tc) booking.rideTax

gstBreakupOf :: Maybe Tax.RideTax -> (RideTaxRates (Maybe Tax.TaxConfig) -> Maybe Tax.TaxConfig) -> DTC.GstBreakup
gstBreakupOf mbRideTax rateOf = case mbRideTax >>= \rt -> if rt.regime == Tax.GST then rateOf rt.riderPayable else Nothing of
  Just t
    | all isNothing [t.cgst, t.sgst, t.igst] -> DTC.GstBreakup {cgstPercentage = fraction (Just (t.tax / 2)), sgstPercentage = fraction (Just (t.tax / 2)), igstPercentage = Nothing}
    | otherwise -> DTC.GstBreakup {cgstPercentage = fraction t.cgst, sgstPercentage = fraction t.sgst, igstPercentage = fraction t.igst}
  Nothing -> DTC.GstBreakup {cgstPercentage = Nothing, sgstPercentage = Nothing, igstPercentage = Nothing}
  where
    fraction = fmap (\pct -> HighPrecMoney (toRational pct / 100))

splitCommissionInclusive :: Currency -> Maybe Tax.RideTax -> HighPrecMoney -> (HighPrecMoney, HighPrecMoney)
splitCommissionInclusive currency mbRideTax gross =
  let split = maybe (Tax.TaxedAmount gross 0) (\rt -> Tax.taxed currency rt.regime rt.driverDeductible.commission (Tax.Inclusive gross)) mbRideTax
   in (split.taxExclusive, split.tax)

gstBreakupFromTax :: Maybe TaxBreakup -> DTC.GstBreakup
gstBreakupFromTax = \case
  Just t
    | t.total > 0 && all isNothing [t.cgst, t.sgst, t.igst] -> DTC.GstBreakup {cgstPercentage = Just 0.5, sgstPercentage = Just 0.5, igstPercentage = Nothing}
    | t.total > 0 -> DTC.GstBreakup {cgstPercentage = share t t.cgst, sgstPercentage = share t t.sgst, igstPercentage = share t t.igst}
  _ -> DTC.GstBreakup {cgstPercentage = Nothing, sgstPercentage = Nothing, igstPercentage = Nothing}
  where
    share t = fmap (/ t.total)
