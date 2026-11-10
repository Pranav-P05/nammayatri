module Domain.Types.FareNetBreakup
  ( FareNetAddition (..),
    FareNetDeduction (..),
    FareNetBreakup (..),
    CommissionAPIEntity (..),
  )
where

import BecknV2.OnDemand.PaymentCharge (PaymentChargeAPIEntity)
import Domain.Types.FarePolicy (FareChargeComponent)
import Domain.Types.PaymentChargeBearer (PaymentChargeType)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, PriceAPIEntity)

data CommissionAPIEntity = CommissionAPIEntity
  { configuredCommission :: HighPrecMoney,
    commissionType :: PaymentChargeType,
    appliesOn :: [FareChargeComponent],
    totalCommission :: PriceAPIEntity,
    commissionTax :: PriceAPIEntity
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

newtype FareNetAddition = FareNetAddition
  { tax :: PriceAPIEntity
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data FareNetDeduction = FareNetDeduction
  { tax :: PriceAPIEntity,
    commission :: PriceAPIEntity,
    paymentCharge :: PriceAPIEntity,
    commissionInfo :: Maybe CommissionAPIEntity,
    paymentChargeInfo :: Maybe PaymentChargeAPIEntity
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data FareNetBreakup = FareNetBreakup
  { total :: PriceAPIEntity,
    net :: PriceAPIEntity,
    addition :: FareNetAddition,
    deduction :: FareNetDeduction
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
