module Statistics (Statistic(..), StatEntry(..), StatInfo(..), StatQFieldData) where

import Control.Applicative
import Data.Ord (comparing)
import Data.Time (diffDays)
import Data.Foldable (minimumBy, maximumBy)
import BoundedEnum
import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded, BoundedEnum)

data StatEntry = StatEntry {
  stat :: Statistic,
  qField :: QField,
  value :: Fixed4
}

type StatQFieldData = (QField, [StatEntry])
type StatInfo = [StatQFieldData]

mean :: (Foldable t, Fractional a) => t a -> a 
mean = (/) <$> sum <*> fromIntegral . length

daysBetween :: (Foldable t) => QField -> t QuoteData -> Fixed4
daysBetween qf quotes = fromIntegral $ abs $ diffDays minQuotes maxQuotes where
  cmp = comparing . field2fun $ qf
  minQuotes = day . minimumBy cmp $ quotes
  maxQuotes = day . maximumBy cmp $ quotes

funcByField :: (Functor f) => (f Fixed4 -> a) -> QField -> f QuoteData -> a 
funcByField func qf = func . fmap (field2fun qf)

computeStatistics :: (Functor t, Foldable t) => Statistic -> (QField ->  t QuoteData -> Fixed4) 
computeStatistics Mean = funcByField mean 
computeStatistics Min = funcByField minimum
computeStatistics Max = funcByField maximum
computeStatistics Days = daysBetween

statInfo :: (Functor t, Foldable t) => t QuoteData -> StatInfo
statInfo quotes = map stQFData range where
  stQFData qf = (qf, [StatEntry st qf v | st <- range , let v = computeStatistics st qf quotes])