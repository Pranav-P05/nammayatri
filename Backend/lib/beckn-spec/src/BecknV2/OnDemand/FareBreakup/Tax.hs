module BecknV2.OnDemand.FareBreakup.Tax
  ( Regime (..),
    TaxConfig (..),
    RideTax (..),
    RiderTaxRates (..),
    riderTaxRatesTagList,
    riderTaxRatesFromTags,
    DriverDeductible (..),
    PlatformDeductible (..),
    TaxBreakups (..),
    TaxBreakup (..),
    FareTaxes,
    TaxedAmount (..),
    TaxableAmount (..),
    taxRateOf,
    taxOn,
    taxed,
    splitTax,
    taxedTotal,
    taxRetainedBySeller,
    mkTaxBreakup,
    taxTotal,
    addTaxBreakup,
    scaleTaxBreakup,
    chargeRegime,
  )
where

import BecknV2.OnDemand.FareBreakup.Core (RideTaxRates)
import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.Utils (getTag)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney (..))

data Regime = GST | VAT | NONE
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data TaxConfig = TaxConfig
  { tax :: Double,
    cgst :: Maybe Double,
    sgst :: Maybe Double,
    igst :: Maybe Double
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data RideTax = RideTax
  { regime :: Regime,
    riderPayable :: RideTaxRates (Maybe TaxConfig),
    driverDeductible :: DriverDeductible (Maybe TaxConfig),
    platformDeductible :: PlatformDeductible (Maybe TaxConfig)
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RiderTaxRates = RiderTaxRates
  { regime :: Regime,
    riderPayable :: RideTaxRates (Maybe TaxConfig)
  }
  deriving (Show, Read, Eq, Generic, ToJSON, FromJSON, ToSchema)

riderTaxRatesTagList :: Maybe RiderTaxRates -> Tags.TagList
riderTaxRatesTagList = maybe [] $ \rates ->
  [Tags.RIDE_TAX_RATES Tags.~= TE.decodeUtf8 (BL.toStrict (A.encode rates))]

riderTaxRatesFromTags :: Maybe [Spec.TagGroup] -> Maybe RiderTaxRates
riderTaxRatesFromTags tagGroups =
  getTag Tags.RIDE_TAX_RATES tagGroups >>= A.decodeStrict . TE.encodeUtf8

data DriverDeductible a = DriverDeductible
  { commission :: a,
    cancellationCommission :: a,
    paymentCharge :: a
  }
  deriving stock (Show, Read, Eq, Generic)

instance ToJSON a => ToJSON (DriverDeductible a)

instance FromJSON a => FromJSON (DriverDeductible a)

instance (Typeable a, ToSchema a) => ToSchema (DriverDeductible a)

newtype PlatformDeductible a = PlatformDeductible
  { paymentCharge :: a
  }
  deriving stock (Show, Read, Eq, Generic)

instance ToJSON a => ToJSON (PlatformDeductible a)

instance FromJSON a => FromJSON (PlatformDeductible a)

instance (Typeable a, ToSchema a) => ToSchema (PlatformDeductible a)

data TaxBreakup = TaxBreakup
  { total :: HighPrecMoney,
    cgst :: Maybe HighPrecMoney,
    sgst :: Maybe HighPrecMoney,
    igst :: Maybe HighPrecMoney
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

type FareTaxes = RideTaxRates (Maybe TaxBreakup)

data TaxBreakups = TaxBreakups
  { riderPayable :: FareTaxes,
    driverDeductible :: DriverDeductible (Maybe TaxBreakup),
    platformDeductible :: PlatformDeductible (Maybe TaxBreakup)
  }
  deriving (Show, Eq, Generic)

data TaxedAmount = TaxedAmount
  { taxExclusive :: HighPrecMoney,
    tax :: HighPrecMoney
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data TaxableAmount = Inclusive HighPrecMoney | Exclusive HighPrecMoney
  deriving (Show, Eq)

taxRateOf :: Regime -> Maybe TaxConfig -> HighPrecMoney
taxRateOf taxRegime mbConfig = maybe 0 pctRatio (taxPct taxRegime mbConfig)

taxOn :: Currency -> Regime -> Maybe TaxConfig -> TaxableAmount -> Maybe TaxBreakup
taxOn currency taxRegime mbConfig taxable = do
  pct <- taxPct taxRegime mbConfig
  splitTax currency taxRegime mbConfig $ case taxable of
    Exclusive base -> base * pctRatio pct
    Inclusive gross -> gross - gross / (1 + pctRatio pct)

taxed :: Currency -> Regime -> Maybe TaxConfig -> TaxableAmount -> TaxedAmount
taxed currency taxRegime mbConfig taxable =
  let taxAmount = taxTotal (taxOn currency taxRegime mbConfig taxable)
   in case taxable of
        Exclusive base -> TaxedAmount {taxExclusive = base, tax = taxAmount}
        Inclusive gross -> TaxedAmount {taxExclusive = gross - taxAmount, tax = taxAmount}

taxedTotal :: TaxedAmount -> HighPrecMoney
taxedTotal amount = amount.taxExclusive + amount.tax

taxRetainedBySeller :: Maybe Regime -> Bool
taxRetainedBySeller = (/= Just GST)

mkTaxBreakup :: Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe HighPrecMoney -> Maybe TaxBreakup
mkTaxBreakup mbTotal cgstPart sgstPart igstPart = (\totalTax -> TaxBreakup {total = totalTax, cgst = cgstPart, sgst = sgstPart, igst = igstPart}) <$> mbTotal

taxTotal :: Maybe TaxBreakup -> HighPrecMoney
taxTotal = maybe 0 (.total)

addTaxBreakup :: Maybe TaxBreakup -> Maybe TaxBreakup -> Maybe TaxBreakup
addTaxBreakup (Just a) (Just b) = Just TaxBreakup {total = a.total + b.total, cgst = addPart a.cgst b.cgst, sgst = addPart a.sgst b.sgst, igst = addPart a.igst b.igst}
  where
    addPart x y = if isNothing x && isNothing y then Nothing else Just (fromMaybe 0 x + fromMaybe 0 y)
addTaxBreakup a b = maybe b Just a

proportionalSplit :: HighPrecMoney -> [Rational] -> [HighPrecMoney]
proportionalSplit _ [] = []
proportionalSplit amount weights
  | weightSum <= 0 = amount : map (const 0) (drop 1 weights)
  | otherwise = [HighPrecMoney (amount.getHighPrecMoney * weight / weightSum) | weight <- clipped]
  where
    clipped = map (max 0) weights
    weightSum = sum clipped

splitGst :: (Rational, Rational, Rational) -> HighPrecMoney -> (Maybe HighPrecMoney, Maybe HighPrecMoney, Maybe HighPrecMoney)
splitGst (c, s, i) taxAmount = case mfilter (/= 0) . Just <$> proportionalSplit taxAmount [c, s, i] of
  [cgstPart, sgstPart, igstPart] -> (cgstPart, sgstPart, igstPart)
  _ -> (Nothing, Nothing, Nothing)

chargeRegime :: TaxConfig -> Regime
chargeRegime t = if any isJust [t.cgst, t.sgst, t.igst] then GST else VAT

taxPct :: Regime -> Maybe TaxConfig -> Maybe Double
taxPct taxRegime mbConfig = case taxRegime of
  NONE -> Nothing
  _ -> mfilter (> 0) ((.tax) <$> mbConfig)

pctRatio :: Double -> HighPrecMoney
pctRatio pct = HighPrecMoney (toRational pct / 100)

splitTax :: Currency -> Regime -> Maybe TaxConfig -> HighPrecMoney -> Maybe TaxBreakup
splitTax _ taxRegime mbConfig taxAmount
  | taxAmount == 0 = Nothing
  | otherwise = case taxRegime of
    NONE -> Nothing
    VAT -> Just TaxBreakup {total = taxAmount, cgst = Nothing, sgst = Nothing, igst = Nothing}
    GST ->
      let (cgstPart, sgstPart, igstPart) = splitGst weights taxAmount
       in Just TaxBreakup {total = taxAmount, cgst = cgstPart, sgst = sgstPart, igst = igstPart}
  where
    weights = case mbConfig of
      Just t | any isJust [t.cgst, t.sgst, t.igst] -> (share t.cgst, share t.sgst, share t.igst)
      _ -> (1, 1, 0)
    share = maybe 0 toRational

scaleTaxBreakup :: Currency -> Rational -> TaxBreakup -> Maybe TaxBreakup
scaleTaxBreakup currency factor t
  | all isNothing [t.cgst, t.sgst, t.igst] = splitTax currency VAT Nothing scaled
  | otherwise = splitTax currency GST (Just TaxConfig {tax = 0, cgst = weight t.cgst, sgst = weight t.sgst, igst = weight t.igst}) scaled
  where
    scaled = HighPrecMoney (t.total.getHighPrecMoney * factor)
    weight = Just . maybe 0 (fromRational . (.getHighPrecMoney))
