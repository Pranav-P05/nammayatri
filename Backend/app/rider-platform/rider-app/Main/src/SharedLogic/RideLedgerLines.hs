module SharedLogic.RideLedgerLines
  ( fareLedgerLines,
    rideRefundableLines,
  )
where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.OfferEntity as DOfferEntity
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRideStatus
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified SharedLogic.FareBreakupInfo as SFareBreakupInfo
import qualified SharedLogic.FareBreakupLines as FBL
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import qualified SharedLogic.RiderFareSettlement as SRFS
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.OfferEntity as QOfferEntity

fareLedgerLines :: HighPrecMoney -> [DFareBreakup.FareBreakup] -> HighPrecMoney -> (HighPrecMoney, Maybe HighPrecMoney) -> ([RidePaymentFinance.LedgerLine], HighPrecMoney)
fareLedgerLines fareAmount breakups discount paymentCharge =
  ( RidePaymentFinance.rideLedgerLines fareAmount (maybe [] (.breakupLines) mbFareLines) paymentCharge,
    maybe discount (\fareLines -> (FBL.applyOffer discount fareLines).appliedDiscount) mbFareLines
  )
  where
    mbFareLines = FBL.fareLinesFromBreakups fareAmount breakups

netOfDiscount :: HighPrecMoney -> RidePaymentFinance.LedgerLine -> RidePaymentFinance.LedgerLine
netOfDiscount discount line
  | line.chargeRef /= RidePaymentFinance.ridePaymentRefRideFare || gross <= 0 = line
  | otherwise = line {RidePaymentFinance.chargeBase = scaled line.chargeBase, RidePaymentFinance.chargeTax = scaled line.chargeTax}
  where
    gross = RidePaymentFinance.ledgerLineTotal line
    remaining = gross - max 0 (min discount gross)
    scaled amount = amount * remaining / gross

rideRefundableLines :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, BeamFlow.BeamFlow m r) => DRide.Ride -> DBooking.Booking -> m [RidePaymentFinance.LedgerLine]
rideRefundableLines ride booking
  | ride.status == DRideStatus.CANCELLED = RidePaymentFinance.settledCancellationLedgerLines <$> RidePaymentFinance.findRidePaymentEntries ride.id.getId
  | otherwise = completedRideRefundableLines ride booking

completedRideRefundableLines :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DRide.Ride -> DBooking.Booking -> m [RidePaymentFinance.LedgerLine]
completedRideRefundableLines ride booking = do
  mbOfferEntity <- QOfferEntity.findByEntityIdAndEntityType ride.id.getId DOfferEntity.RIDE
  breakups <- SFareBreakupInfo.getFareBreakupsWithFallback ride.id.getId DFareBreakup.RIDE (QFareBreakup.findAllByEntityIdAndEntityType ride.id.getId DFareBreakup.RIDE)
  let (ledgerLines, appliedDiscount) =
        fareLedgerLines
          (maybe booking.estimatedFare.amount (.amount) ride.totalFare)
          breakups
          (maybe 0 (.discountAmount) mbOfferEntity)
          (SRFS.rideCustomerBornePaymentCharge ride)
  pure $ map (netOfDiscount appliedDiscount) ledgerLines
