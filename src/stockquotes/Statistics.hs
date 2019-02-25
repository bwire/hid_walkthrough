module Statistics where

import QuoteData

data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded)

data StatEntry = StatEntry {
  stat :: Statistic,
  qField :: QField,
  value :: Fixed4
}

type StatQFieldData = [(QField, [StatEntry])]
type StatInfo = [StatQFieldData]