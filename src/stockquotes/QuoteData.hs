module QuoteData (
  QuoteData(..), 
  QField(..), 
  Fixed4, 
  field2fun
) where

import Data.ByteString.Char8 (unpack)
import Data.Fixed (HasResolution (..), Fixed)
import Data.Time(Day, defaultTimeLocale, parseTimeOrError, parseTimeM)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Safe (readDef)
import Data.Csv (FromNamedRecord, FromField (..))

data E4
  
instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4

instance FromField Fixed4 where
  parseField = return . readDef 0 . unpack

instance FromField Day where
  parseField = parseTimeM False defaultTimeLocale "%Y/%m/%d" . unpack 

data QuoteData = QuoteData {
  day :: Day, 
  close :: Fixed4,
  volume :: Fixed4,
  open :: Fixed4,
  high :: Fixed4,
  low :: Fixed4
} deriving (Generic, FromNamedRecord)

data QField = Open | Close | High | Low | Volume

field2fun :: QField -> QuoteData -> Fixed4
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = volume

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