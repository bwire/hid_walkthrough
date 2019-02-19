{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module QuoteData where

import Data.Fixed (HasResolution (..), Fixed)
import Data.Time(Day, defaultTimeLocale, parseTimeOrError)
import qualified Data.Text as T

data E4
  
instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4

data QuoteData = QuoteData {
  day :: Day, 
  close :: Fixed4,
  volume :: Fixed4,
  open :: Fixed4,
  high :: Fixed4,
  low :: Fixed4
}

text2Quotes :: T.Text -> [QuoteData]
text2Quotes = map (mkQuote . toComponents) . tail . T.lines
  where
    toComponents = map T.unpack . T.splitOn ","
    mkQuote (d : rest@[_, _, _, _, _]) = 
      let day = parseTimeOrError False defaultTimeLocale "%Y/%m%d" d 
          [close, volume, open, high, low] = map read rest
      in QuoteData {..}
    mkComponents _ = error "Incorrect data format"

-- just an example of using wildcards
zeroQuote :: Day -> QuoteData
zeroQuote d = 
  let day = d   
      close = 0
      volume = 0
      open = 0
      high = 0
      low = 0
  in QuoteData {..}