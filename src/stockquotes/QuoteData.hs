{-# LANGUAGE OverloadedStrings #-}

module QuoteData (
  Fixed4,
  QuoteData(..),
  QField(..),
  text2Quotes,
  field2Fun
) where

import Data.Fixed (HasResolution (..), Fixed)
import Data.Time (Day, defaultTimeLocale, parseTimeOrError, parseTimeM)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField(..))
import Safe(readDef)
import Data.ByteString.Char8 (unpack)

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
} deriving (Generic, FromNamedRecord)

instance FromField Day where
  parseField = (parseTimeM False defaultTimeLocale "%Y/%m/%d") . unpack

instance FromField Fixed4 where
  parseField = pure . readDef 0 . unpack

data QField = Open | Close | High | Low | Volume deriving (Show)

field2Fun :: QField -> QuoteData -> Fixed4
field2Fun Open = open
field2Fun Close = close
field2Fun High = high
field2Fun Low = low
field2Fun Volume = volume
  
text2Quotes :: T.Text -> [QuoteData]
text2Quotes = map (mkQuote . toComponents) . tail . T.lines
  where toComponents = map T.unpack . T.split (==',') 
        mkQuote (d : rest@[_,_,_,_,_]) =
          let day = parseTimeOrError False defaultTimeLocale "%Y%m%d" d
              [close, volume, open, high, low] = map read rest
          in QuoteData{..}
        mkQuote _ = error "Incorrect daya format"

