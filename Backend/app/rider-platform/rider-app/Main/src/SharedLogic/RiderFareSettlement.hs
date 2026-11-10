module SharedLogic.RiderFareSettlement
  ( SettlementOffer (..),
    offerOfEntity,
    capturedDiscountOf,
    offerOfSettlement,
    mkPlatformDeductions,
    mkRiderFareSettlement,
    bookingPlatformDeductions,
    bookingCommission,
    bookingPaymentCharge,
    bookingPaymentChargeBearer,
    ridePaymentCharge,
    ridePaymentChargeBearer,
    bookingSettlementWithTotal,
    customerBornePaymentCharge,
    rideCustomerBornePaymentCharge,
  )
where

import BecknV2.OnDemand.FareBreakup.Tax (TaxBreakup)
import BecknV2.OnDemand.FareSettlement (PaymentChargeSettlement (..), SettlementCharge (..))
import BecknV2.OnDemand.PaymentCharge (PlatformDeductions)
import Control.Applicative ((<|>))
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.OfferEntity as DOfferEntity
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import qualified Domain.Types.Ride as DRide
import Domain.Types.RiderFareSettlement
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, Price (..))

data SettlementOffer = SettlementOffer
  { offerDiscount :: Maybe HighPrecMoney,
    capturedDiscount :: Maybe HighPrecMoney,
    cashbackPayout :: Maybe HighPrecMoney
  }

offerOfEntity :: Maybe DOfferEntity.OfferEntity -> SettlementOffer
offerOfEntity mbOfferEntity =
  SettlementOffer
    { offerDiscount = (.discountAmount) <$> mbOfferEntity,
      capturedDiscount = capturedDiscountOf <$> mbOfferEntity,
      cashbackPayout = (.payoutAmount) <$> mbOfferEntity
    }

capturedDiscountOf :: DOfferEntity.OfferEntity -> HighPrecMoney
capturedDiscountOf offerEntity = max 0 (offerEntity.amountSaved - offerEntity.payoutAmount)

offerOfSettlement :: Maybe RiderFareSettlement -> SettlementOffer
offerOfSettlement mbSettlement =
  SettlementOffer
    { offerDiscount = mbSettlement >>= (.offerDiscount),
      capturedDiscount = mbSettlement >>= (.capturedDiscount),
      cashbackPayout = mbSettlement >>= (.cashbackPayout)
    }

mkPlatformDeductions :: Maybe HighPrecMoney -> Maybe TaxBreakup -> Maybe HighPrecMoney -> Maybe TaxBreakup -> Maybe PaymentChargeBearer -> Maybe PlatformDeductions -> RiderPlatformDeductions
mkPlatformDeductions mbCommission mbCommissionTaxBreakup mbPaymentCharge mbPaymentChargeTaxBreakup mbBearer mbDeductedAtPayment =
  RiderPlatformDeductions
    { commission = (\amount -> SettlementCharge {amount, taxBreakup = mbCommissionTaxBreakup}) <$> mbCommission,
      paymentCharge = do
        amount <- mbPaymentCharge
        bearer <- mbBearer
        pure PaymentChargeSettlement {amount, taxBreakup = mbPaymentChargeTaxBreakup, bearer},
      deductedAtPayment = mbDeductedAtPayment
    }

mkRiderFareSettlement :: Price -> SettlementOffer -> RiderPlatformDeductions -> RiderFareSettlement
mkRiderFareSettlement totalFare offer deductions =
  RiderFareSettlement
    { currency = totalFare.currency,
      total = totalFare.amount,
      payable = max 0 (totalFare.amount - fromMaybe 0 (offer.capturedDiscount <|> offer.offerDiscount)),
      offerDiscount = offer.offerDiscount,
      capturedDiscount = offer.capturedDiscount,
      cashbackPayout = offer.cashbackPayout,
      customerPaymentCharge = customerCharge =<< deductions.paymentCharge,
      platformDeductions = deductions
    }
  where
    customerCharge :: PaymentChargeSettlement -> Maybe SettlementCharge
    customerCharge charge
      | charge.bearer == PAYMENT_CUSTOMER = Just SettlementCharge {amount = charge.amount, taxBreakup = charge.taxBreakup}
      | otherwise = Nothing

bookingPlatformDeductions :: DBooking.Booking -> RiderPlatformDeductions
bookingPlatformDeductions booking = maybe (mkPlatformDeductions booking.commission Nothing booking.paymentCharge Nothing booking.paymentChargeBearer Nothing) (.platformDeductions) booking.estimatedFareSettlement

bookingCommission :: DBooking.Booking -> Maybe HighPrecMoney
bookingCommission booking = ((.amount) <$> (booking.estimatedFareSettlement >>= (.platformDeductions.commission))) <|> booking.commission

bookingPaymentCharge :: DBooking.Booking -> Maybe HighPrecMoney
bookingPaymentCharge booking = ((.amount) <$> (booking.estimatedFareSettlement >>= (.platformDeductions.paymentCharge))) <|> booking.paymentCharge

bookingPaymentChargeBearer :: DBooking.Booking -> Maybe PaymentChargeBearer
bookingPaymentChargeBearer booking = ((.bearer) <$> (booking.estimatedFareSettlement >>= (.platformDeductions.paymentCharge))) <|> booking.paymentChargeBearer

ridePaymentCharge :: DRide.Ride -> Maybe HighPrecMoney
ridePaymentCharge ride = ((.amount) <$> (ride.fareSettlement >>= (.platformDeductions.paymentCharge))) <|> ride.paymentCharge

ridePaymentChargeBearer :: DRide.Ride -> Maybe PaymentChargeBearer
ridePaymentChargeBearer ride = ((.bearer) <$> (ride.fareSettlement >>= (.platformDeductions.paymentCharge))) <|> ride.paymentChargeBearer

bookingSettlementWithTotal :: DBooking.Booking -> Price -> RiderFareSettlement
bookingSettlementWithTotal booking newFare = mkRiderFareSettlement newFare (offerOfSettlement booking.estimatedFareSettlement) (bookingPlatformDeductions booking)

customerBornePaymentCharge :: Maybe HighPrecMoney -> Maybe TaxBreakup -> Maybe PaymentChargeBearer -> (HighPrecMoney, Maybe HighPrecMoney)
customerBornePaymentCharge mbCharge mbChargeTaxBreakup mbBearer
  | mbBearer == Just PAYMENT_CUSTOMER = (fromMaybe 0 mbCharge, (.total) <$> mbChargeTaxBreakup)
  | otherwise = (0, Nothing)

rideCustomerBornePaymentCharge :: DRide.Ride -> (HighPrecMoney, Maybe HighPrecMoney)
rideCustomerBornePaymentCharge ride = customerBornePaymentCharge (ridePaymentCharge ride) (ride.fareSettlement >>= (.platformDeductions.paymentCharge) >>= (.taxBreakup)) (ridePaymentChargeBearer ride)
