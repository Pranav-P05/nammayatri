module SharedLogic.PaymentCharge
  ( PaymentChargeDetails (..),
    PaymentChargeQuote (..),
    quotePaymentCharge,
    resolvePaymentChargeDetails,
    paymentChargeDetailsTagList,
    PlatformDeductions (..),
    platformDeductionsAtPayment,
    platformDeductionsTagList,
    riderPaymentChargeDetails,
    mkGrossAmount,
    mkChargeGross,
    grossFromLines,
    grossOfSettledFare,
    fareTaxOfLines,
    chargeAmountFromPayable,
    LeviedPaymentCharge (..),
    levyPaymentCharge,
    PlatformComponent (..),
    platformComponents,
    ridePlatformComponents,
    applicationFeeOf,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import BecknV2.OnDemand.PaymentCharge (PaymentChargeDetails (..), PaymentChargeQuote (..), PlatformDeductions (..), paymentChargeBorneBy, paymentChargeDetailsTagList, paymentChargeGross, paymentChargeRateBorneBy, platformCollectsPaymentCharge, platformDeductionsAtPayment, platformDeductionsTagList, quotePaymentCharge)
import Control.Applicative ((<|>))
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPMExtra
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import Lib.ConfigPilot.Interface.Types (getConfigList)
import qualified SharedLogic.RiderFareSettlement as SRFS
import Storage.ConfigPilot.Config.MerchantPaymentMethod (MerchantPaymentMethodDimensions (..))

paymentChargeDetailsOf :: DMPM.MerchantPaymentMethod -> Maybe PaymentChargeDetails
paymentChargeDetailsOf paymentMethod = do
  chargeType <- paymentMethod.paymentChargeType
  chargeValue <- paymentMethod.paymentCharge
  bearer <- paymentMethod.paymentChargeBearer
  pure PaymentChargeDetails {chargeType, chargeValue, chargeTax = paymentMethod.paymentChargeTax, bearer}

resolvePaymentChargeDetails ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPMExtra.PaymentInstrument ->
  m (Maybe PaymentChargeDetails)
resolvePaymentChargeDetails merchantOperatingCityId mbInstrument = case mbInstrument of
  Nothing -> pure Nothing
  Just instrument -> do
    paymentMethods <- getConfigList (MerchantPaymentMethodDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, configId = Nothing})
    pure $ find ((== instrument) . (.paymentInstrument)) paymentMethods >>= paymentChargeDetailsOf

riderPaymentChargeDetails ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe DMPMExtra.PaymentInstrument ->
  m (Maybe PaymentChargeDetails)
riderPaymentChargeDetails merchantOperatingCityId mbPaymentInstrument =
  resolvePaymentChargeDetails merchantOperatingCityId (Just (fromMaybe DMPMExtra.Cash mbPaymentInstrument))

mkGrossAmount :: Currency -> Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
mkGrossAmount grossCurrency mbPaymentCharge taxInclusiveAmount =
  let charge = paymentChargeBorneBy PAYMENT_CUSTOMER grossCurrency mbPaymentCharge taxInclusiveAmount
   in taxInclusiveAmount + charge.taxExclusive + charge.tax

mkChargeGross :: Currency -> Maybe Tax.RiderTaxRates -> Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
mkChargeGross grossCurrency mbRiderTaxRates mbPaymentCharge chargeAmount =
  let chargeTaxed = maybe (Tax.TaxedAmount chargeAmount 0) (\rates -> Tax.taxed grossCurrency rates.regime (FB.rideFare rates.riderPayable) (Tax.Exclusive chargeAmount)) mbRiderTaxRates
   in Tax.taxedTotal chargeTaxed * (1 + paymentChargeRateBorneBy PAYMENT_CUSTOMER mbPaymentCharge)

grossFromLines :: Currency -> Maybe PaymentChargeDetails -> HighPrecMoney -> [FB.BreakupLine] -> HighPrecMoney
grossFromLines grossCurrency mbPaymentCharge fare breakupLines =
  let (chargeNet, chargeTax) = (FB.componentLines breakupLines).paymentCharge
   in if chargeNet + chargeTax > 0
        then fare
        else mkGrossAmount grossCurrency mbPaymentCharge fare

grossOfSettledFare :: Currency -> Maybe PaymentChargeDetails -> Maybe PaymentChargeBearer -> HighPrecMoney -> [FB.BreakupLine] -> HighPrecMoney
grossOfSettledFare grossCurrency mbPaymentCharge mbSettledBearer fare breakupLines
  | isJust mbSettledBearer = fare
  | otherwise = grossFromLines grossCurrency mbPaymentCharge fare breakupLines

fareTaxOfLines :: [FB.BreakupLine] -> HighPrecMoney
fareTaxOfLines breakupLines = FB.fareTaxAmount breakupLines - snd (FB.componentLines breakupLines).paymentCharge

chargeAmountFromPayable :: Currency -> Maybe Tax.RiderTaxRates -> Maybe PaymentChargeDetails -> HighPrecMoney -> HighPrecMoney
chargeAmountFromPayable _ mbRiderTaxRates mbPaymentCharge payable =
  payable / ((1 + rideTaxRate) * (1 + paymentChargeRateBorneBy PAYMENT_CUSTOMER mbPaymentCharge))
  where
    rideTaxRate = maybe 0 (\rates -> Tax.taxRateOf rates.regime (FB.rideFare rates.riderPayable)) mbRiderTaxRates

-- | The payment charge levied on an amount outside the original fare, split the
--   way the invoice and ledger need it.
data LeviedPaymentCharge = LeviedPaymentCharge
  { -- | The charge and its VAT, for invoice lines and ledger legs.
    chargeNet :: HighPrecMoney,
    chargeVat :: HighPrecMoney,
    -- | What the rider is captured on top of the amount.
    riderGrossUp :: HighPrecMoney,
    -- | The gross charge the platform collects; added to the commission for the
    --   payment intent's application fee.
    collectedPaymentCharge :: HighPrecMoney
  }

-- | Levy the ride's payment charge on an amount outside the original fare.
--
--   Mirrors the fare exactly rather than inventing a policy. Under
--   PAYMENT_CUSTOMER the rider pays the charge on top and the platform collects
--   it, so the driver still receives the full amount; under PAYMENT_DRIVER the
--   rider pays only the amount and the driver funds the fee out of it; under
--   PAYMENT_PLATFORM the platform absorbs it. So a tip or cancellation fee is
--   not silently cheaper to process than a fare of the same size.
levyPaymentCharge :: Maybe PaymentChargeQuote -> LeviedPaymentCharge
levyPaymentCharge = \case
  Just quote -> case quote.bearer of
    PAYMENT_CUSTOMER -> LeviedPaymentCharge quote.net quote.tax (paymentChargeGross quote) (paymentChargeGross quote)
    PAYMENT_DRIVER -> LeviedPaymentCharge quote.net quote.tax 0 (paymentChargeGross quote)
    PAYMENT_PLATFORM -> none
  Nothing -> none
  where
    none = LeviedPaymentCharge 0 0 0 0

data PlatformComponent = COMMISSION | PAYMENT_CHARGE
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

platformComponents :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe PaymentChargeBearer -> [(PlatformComponent, HighPrecMoney)]
platformComponents mbCommission mbCharge mbBearer =
  [ (COMMISSION, fromMaybe 0 mbCommission),
    (PAYMENT_CHARGE, if maybe False platformCollectsPaymentCharge mbBearer then fromMaybe 0 mbCharge else 0)
  ]

applicationFeeOf :: [(PlatformComponent, HighPrecMoney)] -> HighPrecMoney
applicationFeeOf = sum . map snd

ridePlatformComponents :: DRide.Ride -> [(PlatformComponent, HighPrecMoney)]
ridePlatformComponents ride = platformComponents (((.amount) <$> (ride.fareSettlement >>= (.platformDeductions.commission))) <|> ride.commission) (SRFS.ridePaymentCharge ride) (SRFS.ridePaymentChargeBearer ride)
