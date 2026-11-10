module SharedLogic.RideFareBreakup
  ( buildRideFareBreakup,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import BecknV2.OnDemand.FareBreakup.SubItems (FareBreakupLine (..), FareUIComponent (..), FareUIComponentType (..), FareUISection (..), FareUISubItem (..), computeFareBreakupLines)
import qualified Data.Map.Strict as Map
import Domain.Types.Extra.RiderConfig (CustomerFareBreakupNode)
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Kernel.External.Types as Lang
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.Translations as CQTranslations
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))

buildRideFareBreakup ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  Id DMOC.MerchantOperatingCity ->
  Lang.Language ->
  Currency ->
  Maybe [CustomerFareBreakupNode] ->
  [DFareBreakup.FareBreakup] ->
  HighPrecMoney ->
  HighPrecMoney ->
  m [FareUIComponent]
buildRideFareBreakup cityId language currency mbConfig fareBreakups discount tipFallback = do
  quotedSubItems <- mapM mkLineSubItem $ case mbConfig of
    Just config -> computeFareBreakupLines currency config quotedAmounts
    Nothing -> [FareBreakupLine title amount [] | (title, amount) <- Map.toList quotedAmounts, amount /= 0]
  taxSubItems <- forM taxLines $ \(title, amount, splits) -> do
    splitItems <- forM splits $ \(splitTitle, splitAmount) -> mkSubItem splitTitle splitAmount Earning []
    mkSubItem title amount Earning splitItems
  catMaybes
    <$> sequence
      [ mkComponent "AMOUNT_QUOTED_BY_SELLER" Earning quotedTotal quotedSubItems,
        mkComponent "DISCOUNT" Deduction discount [],
        mkComponent "TIP" Earning tip [],
        mkComponent "TAX" Earning taxTotal taxSubItems
      ]
  where
    lines' = [(fb.description, fb.amount.amount) | fb <- FB.withoutHiddenLines (.description) fareBreakups]
    tipTitle = show Enums.BUYER_ADDITIONAL_AMOUNT
    tipFromLines = sum [amount | (title, amount) <- lines', title == tipTitle]
    tip = if tipFromLines > 0 then tipFromLines else tipFallback
    isQuoted title = not (FB.isTaxTitle title) && title /= tipTitle
    quotedAmounts = Map.fromListWith (+) [(title, amount) | (title, amount) <- lines', isQuoted title]
    quotedTotal = sum (Map.elems quotedAmounts)
    amountOf titles = sum [amount | (title, amount) <- lines', title `elem` titles]
    taxLines =
      [ (FB.tagTitle tags.tax, total, [(FB.tagTitle tag, amountOf (FB.tagTitles tag)) | tag <- [tags.cgst, tags.sgst, tags.igst], amountOf (FB.tagTitles tag) > 0])
        | tags <- toList FB.taxTags,
          let total = amountOf (FB.tagTitles tags.tax),
          total > 0
      ]
    taxTotal = sum [total | (_, total, _) <- taxLines]
    price amount = PriceAPIEntity (roundAmountByCurrency' currency (abs amount)) currency
    translate key = fmap (.message) <$> getConfig (TranslationDimensions {merchantOperatingCityId = Just cityId.getId, messageKey = key, language = Just language}) (Just (CQTranslations.findByMerchantOpCityIdMessageKeyLanguageWithInMemcache cityId key language))
    mkSubItem key amount componentType children = do
      label <- translate key
      pure FareUISubItem {title = key, price = price amount, uiTranslation = label, _type = componentType, subItems = children}
    mkLineSubItem line = do
      children <- mapM mkLineSubItem line.subItems
      mkSubItem line.key line.amount (if line.amount < 0 then Deduction else Earning) children
    mkComponent key componentType amount children
      | amount <= 0 = pure Nothing
      | otherwise = do
        label <- translate key
        pure $ Just FareUIComponent {title = key, price = price amount, uiTranslation = label, section = FareBreakup, _type = componentType, subItems = children}
