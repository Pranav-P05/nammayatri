module SharedLogic.FareBreakupLines
  ( FareLines (..),
    FareDiscount (..),
    fareLinesFromPairs,
    fareLinesFromRateCard,
    fareLinesFromBreakups,
    offerBasePrice,
    applyOffer,
  )
where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.FareBreakupInfo (FareBreakupInfoItem)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)

data FareLines = FareLines
  { fare :: HighPrecMoney,
    breakupLines :: [FB.BreakupLine]
  }
  deriving (Show)

data FareDiscount = FareDiscount
  { appliedDiscount :: HighPrecMoney,
    totalBefore :: HighPrecMoney,
    totalAfter :: HighPrecMoney
  }
  deriving (Show, Generic)

fareLinesFromPairs :: HighPrecMoney -> [(Text, HighPrecMoney)] -> Maybe FareLines
fareLinesFromPairs fareAmount pairs
  | null pairs = Nothing
  | otherwise = Just (FareLines fareAmount (map (uncurry FB.BreakupLine) pairs))

fareLinesFromRateCard :: HighPrecMoney -> Maybe [FareBreakupInfoItem] -> [(Text, HighPrecMoney)] -> Maybe FareLines
fareLinesFromRateCard fareAmount mbFareBreakups breakupPairs = fareLinesFromPairs fareAmount (maybe breakupPairs (map (\item -> (item.description, item.amount))) mbFareBreakups)

fareLinesFromBreakups :: HighPrecMoney -> [DFareBreakup.FareBreakup] -> Maybe FareLines
fareLinesFromBreakups fareAmount = fareLinesFromPairs fareAmount . map (\fb -> (fb.description, fb.amount.amount))

offerBasePrice :: FareLines -> HighPrecMoney
offerBasePrice fl = FB.discountableAmount fl.fare fl.breakupLines

applyOffer :: HighPrecMoney -> FareLines -> FareDiscount
applyOffer discount fl =
  let applied = FB.clampDiscount fl.fare fl.breakupLines discount
   in FareDiscount
        { appliedDiscount = applied,
          totalBefore = fl.fare,
          totalAfter = fl.fare - applied
        }
