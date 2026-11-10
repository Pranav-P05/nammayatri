module Domain.Types.RiderFareSettlement where

import BecknV2.OnDemand.FareSettlement (PaymentChargeSettlement, SettlementCharge)
import BecknV2.OnDemand.PaymentCharge (PlatformDeductions)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)

data RiderPlatformDeductions = RiderPlatformDeductions
  { commission :: Maybe SettlementCharge,
    paymentCharge :: Maybe PaymentChargeSettlement,
    deductedAtPayment :: Maybe PlatformDeductions
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

data RiderFareSettlement = RiderFareSettlement
  { currency :: Currency,
    total :: HighPrecMoney,
    payable :: HighPrecMoney,
    offerDiscount :: Maybe HighPrecMoney,
    capturedDiscount :: Maybe HighPrecMoney,
    cashbackPayout :: Maybe HighPrecMoney,
    customerPaymentCharge :: Maybe SettlementCharge,
    platformDeductions :: RiderPlatformDeductions
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)
