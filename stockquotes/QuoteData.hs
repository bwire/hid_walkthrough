module QuoteData where

import Data.Fixed (HasResolution (..), Fixed)

data E4
  
instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4