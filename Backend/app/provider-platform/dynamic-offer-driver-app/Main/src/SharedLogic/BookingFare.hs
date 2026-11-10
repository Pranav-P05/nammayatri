module SharedLogic.BookingFare
  ( BookingFare (..),
    mkBookingFare,
    rebuildBookingFare,
    applyBookingFare,
    refreshOfferDiscount,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails, PlatformDeductions)
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.ConditionalCharges as DTCC
import qualified Domain.Types.FareParameters as DFP
import Domain.Types.FarePolicy (FareChargeConfig)
import qualified Domain.Types.FareSettlement as DFS
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer)
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified SharedLogic.FareCalculator as FC
import qualified SharedLogic.RideTax as RideTax
import Tools.Metrics (CoreMetrics)

data BookingFare = BookingFare
  { fareParams :: DFP.FareParameters,
    estimatedFare :: HighPrecMoney,
    settlement :: DFS.FareSettlement,
    rideTax :: Tax.RideTax,
    discountAmount :: Maybe HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    cancellationCommission :: Maybe HighPrecMoney,
    paymentCharge :: Maybe HighPrecMoney,
    paymentChargeBearer :: Maybe PaymentChargeBearer,
    isSafetyPlus :: Bool,
    isPetRide :: Bool,
    bookingDeposit :: Maybe HighPrecMoney,
    estimatedCongestionCharge :: Maybe HighPrecMoney
  }

mkBookingFare ::
  ( HasField "rideTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "tollTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "parkingTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "commissionTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "commissionChargeConfig" fp (Maybe FareChargeConfig),
    HasField "conditionalCharges" fp [DTCC.ConditionalCharges]
  ) =>
  DTC.TransporterConfig ->
  Maybe fp ->
  Maybe PaymentChargeDetails ->
  Maybe FC.CancellationDuesCommission ->
  Maybe PlatformDeductions ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  DFP.FareParameters ->
  BookingFare
mkBookingFare transporterConfig mbFarePolicy mbPaymentChargeDetails mbDuesCommission mbDeductedAtPayment mbRawDiscount fare quotedFareParams =
  BookingFare
    { fareParams = chargedFareParams,
      estimatedFare = chargeRes.adjustedFare,
      settlement = fareSettlement,
      rideTax = resolvedRideTax,
      discountAmount = clampedDiscount,
      commission = (.amount) <$> fareSettlement.commission,
      cancellationCommission = (.amount) <$> fareSettlement.cancellationDuesCommission,
      paymentCharge = chargeRes.paymentCharge,
      paymentChargeBearer = chargeRes.paymentChargeBearer,
      isSafetyPlus = DTCC.SAFETY_PLUS_CHARGES `elem` map (.chargeCategory) chargedFareParams.conditionalCharges,
      isPetRide = isJust chargedFareParams.petCharges,
      bookingDeposit = (.charge) <$> (find (\cc -> cc.chargeCategory == DTCC.BOOKING_DEPOSIT) . (.conditionalCharges) =<< mbFarePolicy),
      estimatedCongestionCharge = chargedFareParams.congestionCharge
    }
  where
    clampedDiscount = FC.clampDiscountToDiscountable quotedFareParams mbRawDiscount
    chargeRes = FC.paymentChargeResult mbPaymentChargeDetails clampedDiscount fare quotedFareParams
    chargedFareParams = chargeRes.adjustedFareParams
    resolvedRideTax = RideTax.resolveTaxRates transporterConfig mbFarePolicy mbPaymentChargeDetails
    rates = (FC.fareChargeRates transporterConfig mbFarePolicy mbPaymentChargeDetails clampedDiscount chargedFareParams.currency) {FC.cancellationDuesCommission = mbDuesCommission, FC.deductedAtPayment = mbDeductedAtPayment}
    fareSettlement = snd (FC.fareSum rates chargedFareParams Nothing)

rebuildBookingFare ::
  ( HasField "rideTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "tollTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "parkingTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "commissionTaxConfig" fp (Maybe Tax.TaxConfig),
    HasField "commissionChargeConfig" fp (Maybe FareChargeConfig),
    HasField "conditionalCharges" fp [DTCC.ConditionalCharges]
  ) =>
  DTC.TransporterConfig ->
  Maybe fp ->
  DRB.Booking ->
  Maybe HighPrecMoney ->
  HighPrecMoney ->
  DFP.FareParameters ->
  BookingFare
rebuildBookingFare transporterConfig mbFarePolicy booking =
  mkBookingFare transporterConfig mbFarePolicy booking.paymentChargeDetails (FC.bookingCancellationDuesCommission booking) (booking.estimatedFareSettlement >>= (.deductedAtPayment))

applyBookingFare :: BookingFare -> DRB.Booking -> DRB.Booking
applyBookingFare bookingFare booking =
  booking
    { DRB.fareParams = bookingFare.fareParams,
      DRB.estimatedFare = bookingFare.estimatedFare,
      DRB.estimatedFareSettlement = Just bookingFare.settlement,
      DRB.rideTax = Just bookingFare.rideTax,
      DRB.discountAmount = bookingFare.discountAmount,
      DRB.commission = bookingFare.commission,
      DRB.cancellationCommission = bookingFare.cancellationCommission,
      DRB.paymentCharge = bookingFare.paymentCharge,
      DRB.paymentChargeBearer = bookingFare.paymentChargeBearer,
      DRB.isSafetyPlus = bookingFare.isSafetyPlus,
      DRB.isPetRide = bookingFare.isPetRide,
      DRB.bookingDeposit = bookingFare.bookingDeposit,
      DRB.estimatedCongestionCharge = bookingFare.estimatedCongestionCharge
    }

refreshOfferDiscount ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "appBackendBapInternal" r CallBAPInternal.AppBackendBapInternal,
    HasRequestId r
  ) =>
  DRB.Booking ->
  Maybe HighPrecMoney ->
  Maybe DVST.ServiceTierType ->
  DFP.FareParameters ->
  DFP.FareParameters ->
  m (Maybe HighPrecMoney)
refreshOfferDiscount booking currentDiscount mbNewServiceTier currentFareParams newFareParams
  | isNothing currentDiscount = pure Nothing
  | not serviceTierChanged && breakupLines currentFareParams == breakupLines newFareParams = pure currentDiscount
  | otherwise = do
    appBackendBapInternal <- asks (.appBackendBapInternal)
    let newFareParamsWithoutCharge = withoutPaymentCharge newFareParams
        reqBody =
          CallBAPInternal.OfferDiscountReq
            { fareAmount = Just (fst (FC.fareSum (FC.bookingFareChargeRates Nothing Nothing booking) newFareParamsWithoutCharge Nothing)),
              fareBreakups = Just (breakupLines newFareParams),
              serviceTier = if serviceTierChanged then mbNewServiceTier else Nothing
            }
    result <-
      withTryCatch "refreshOfferDiscount" $
        CallBAPInternal.getOfferDiscount appBackendBapInternal.apiKey appBackendBapInternal.url booking.id.getId reqBody
    case result of
      Right resp -> pure resp.discountAmount
      Left err -> do
        logError $ "Error refreshing offer discount for booking " <> booking.id.getId <> ", keeping current discount: " <> show err
        pure currentDiscount
  where
    serviceTierChanged = maybe False (/= booking.vehicleServiceTier) mbNewServiceTier
    withoutPaymentCharge fp = (FC.paymentChargeResult Nothing Nothing 0 fp).adjustedFareParams
    breakupLines = FC.mkFareParamsBreakups True identity FB.BreakupLine . withoutPaymentCharge
