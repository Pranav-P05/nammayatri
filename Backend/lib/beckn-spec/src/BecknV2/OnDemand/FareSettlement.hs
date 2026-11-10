module BecknV2.OnDemand.FareSettlement
  ( SettlementCharge (..),
    PaymentChargeSettlement (..),
    SettlementTags (..),
    settlementTagGroups,
    settlementFromTags,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails, paymentChargeDetailsFromTags, paymentChargeDetailsTagList)
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils (getTag)
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, highPrecMoneyFromText, highPrecMoneyToText)

data SettlementCharge = SettlementCharge
  { amount :: HighPrecMoney,
    taxBreakup :: Maybe Tax.TaxBreakup
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data PaymentChargeSettlement = PaymentChargeSettlement
  { amount :: HighPrecMoney,
    taxBreakup :: Maybe Tax.TaxBreakup,
    bearer :: PaymentChargeBearer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SettlementTags = SettlementTags
  { commission :: Maybe HighPrecMoney,
    commissionTaxBreakup :: Maybe Tax.TaxBreakup,
    paymentCharge :: Maybe HighPrecMoney,
    paymentChargeTaxBreakup :: Maybe Tax.TaxBreakup,
    paymentChargeDetails :: Maybe PaymentChargeDetails
  }

data ChargeTags = ChargeTags
  { amountTag :: Tags.BecknTag,
    taxTag :: Tags.BecknTag,
    cgstTag :: Tags.BecknTag,
    sgstTag :: Tags.BecknTag,
    igstTag :: Tags.BecknTag
  }

commissionTags :: ChargeTags
commissionTags = ChargeTags Tags.COMMISSION Tags.COMMISSION_TAX Tags.COMMISSION_CGST Tags.COMMISSION_SGST Tags.COMMISSION_IGST

paymentChargeTags :: ChargeTags
paymentChargeTags = ChargeTags Tags.PAYMENT_CHARGE Tags.PAYMENT_CHARGE_TAX Tags.PAYMENT_CHARGE_CGST Tags.PAYMENT_CHARGE_SGST Tags.PAYMENT_CHARGE_IGST

settlementTagGroups :: SettlementTags -> Maybe [Spec.TagGroup]
settlementTagGroups settlement =
  if null tags then Nothing else Tags.buildTagGroups tags
  where
    tags =
      chargeTagList commissionTags settlement.commission settlement.commissionTaxBreakup
        <> chargeTagList paymentChargeTags settlement.paymentCharge settlement.paymentChargeTaxBreakup
        <> paymentChargeDetailsTagList settlement.paymentChargeDetails
    chargeTagList charge mbAmount mbTaxBreakup =
      catMaybes
        [ (charge.amountTag Tags.~=) . highPrecMoneyToText <$> mbAmount,
          (charge.taxTag Tags.~=) . highPrecMoneyToText . (.total) <$> mbTaxBreakup,
          (charge.cgstTag Tags.~=) . highPrecMoneyToText <$> (mbTaxBreakup >>= (.cgst)),
          (charge.sgstTag Tags.~=) . highPrecMoneyToText <$> (mbTaxBreakup >>= (.sgst)),
          (charge.igstTag Tags.~=) . highPrecMoneyToText <$> (mbTaxBreakup >>= (.igst))
        ]

settlementFromTags :: Maybe [Spec.TagGroup] -> SettlementTags
settlementFromTags tagGroups =
  SettlementTags
    { commission = money commissionTags.amountTag,
      commissionTaxBreakup = taxBreakupOf commissionTags,
      paymentCharge = money paymentChargeTags.amountTag,
      paymentChargeTaxBreakup = taxBreakupOf paymentChargeTags,
      paymentChargeDetails = paymentChargeDetailsFromTags tagGroups
    }
  where
    money tag = getTag tag tagGroups >>= highPrecMoneyFromText
    taxBreakupOf charge = Tax.mkTaxBreakup (money charge.taxTag) (money charge.cgstTag) (money charge.sgstTag) (money charge.igstTag)
