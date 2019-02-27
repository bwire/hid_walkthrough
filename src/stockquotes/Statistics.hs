module Statistics where

import Control.Applicative
import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded)

data StatEntry = StatEntry {
  stat :: Statistic,
  qField :: QField,
  value :: Fixed4
}

type StatQFieldData = (QField, [StatEntry])
type StatInfo = [StatQFieldData]

mean :: (Foldable t, Fractional a) => t a -> a 
mean = (/) <$> sum <*> fromIntegral . length

daysBetween = undefined

funcByField :: (Functor f) => (f Fixed4 -> a) -> QField -> f QuoteData -> a 
funcByField func qf = func . fmap (field2fun qf)

computeStatistics :: (Functor t, Foldable t) => Statistic -> (QField ->  t QuoteData -> Fixed4) 
computeStatistics Mean = funcByField mean 
computeStatistics Min = funcByField minimum
computeStatistics Max = funcByField maximum
computeStatistics Days = funcByField daysBetween