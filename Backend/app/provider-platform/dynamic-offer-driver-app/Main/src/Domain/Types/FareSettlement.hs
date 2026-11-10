module Domain.Types.FareSettlement
  ( FareSettlement (..),
    TdsSettlement (..),
    CancellationSettlement (..),
  )
where

import BecknV2.OnDemand.FareBreakup.Tax (TaxBreakup)
import BecknV2.OnDemand.FareSettlement (PaymentChargeSettlement, SettlementCharge)
import BecknV2.OnDemand.PaymentCharge (PlatformDeductions)
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)

data TdsSettlement = TdsSettlement
  { amount :: HighPrecMoney,
    rate :: Maybe Double,
    processedAt :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data FareSettlement = FareSettlement
  { currency :: Currency,
    total :: HighPrecMoney,
    net :: HighPrecMoney,
    taxAdded :: HighPrecMoney,
    taxDeducted :: HighPrecMoney,
    discount :: Maybe HighPrecMoney,
    commission :: Maybe SettlementCharge,
    cancellationDuesCommission :: Maybe SettlementCharge,
    paymentCharge :: Maybe PaymentChargeSettlement,
    tds :: Maybe TdsSettlement,
    deductedAtPayment :: Maybe PlatformDeductions
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data CancellationSettlement = CancellationSettlement
  { chargesOnCancelTax :: Maybe HighPrecMoney,
    feeTaxBreakupIfCancelled :: Maybe TaxBreakup,
    commissionIfCancelled :: Maybe SettlementCharge
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
