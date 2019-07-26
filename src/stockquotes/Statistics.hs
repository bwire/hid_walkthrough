module Statistics (
  Statistic(..),
  StatEntry(..),
  StatQFieldData
) where

import Data.Time (diffDays)
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import BoundedEnum 
import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded, BoundedEnum)

data StatEntry = StatEntry {
    stat :: Statistic,
    qfield :: QField,
    value :: Fixed4
  }

type StatQFieldData = (QField, [StatEntry])
type StatInfo = [StatQFieldData]

mean :: (Fractional a, Foldable t) => t a -> a
mean = (/) <$> sum <*> fromIntegral . length

daysBetween :: (Foldable t) => QField -> t QuoteData -> Fixed4
daysBetween qf quotes = fromIntegral $ abs $ diffDays minQuotes maxQuotes where
  cmp = comparing $ field2Fun qf
  minQuotes = day $ minimumBy cmp quotes
  maxQuotes = day $ maximumBy cmp quotes

funcByField :: (Functor f) => (f Fixed4 -> a) -> QField -> f QuoteData -> a
funcByField func qf = func . fmap (field2Fun qf) 

computeStatistic :: (Foldable f, Functor f) => Statistic -> QField -> f QuoteData -> Fixed4
computeStatistic Mean = funcByField mean
computeStatistic Min = funcByField minimum
computeStatistic Max = funcByField maximum 
computeStatistic Days = daysBetween

statInfo :: (Functor t, Foldable t) => t QuoteData -> StatInfo
statInfo quotes = map stQFData range where
  stQFData qf = (qf, [StatEntry st qf v | st <- range, let v = computeStatistic st qf quotes])