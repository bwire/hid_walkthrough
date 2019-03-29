module Main where

import Control.Monad (when)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Csv (decodeByName)

import Params
import QuoteData
import Statistics
import statReport
import Charts

-- temporary definition
data QuoteDataCollection
data StatInfo

work :: Params -> IO ()
work params = undefined

readQuotes :: FilePath -> IO QuoteDataCollection
readQuotes = undefined

statInfo :: QuoteDataCollection -> StatInfo
statInfo = undefined

statReport :: StatInfo -> Text
statReport = undefined

main :: IO ()
main = cmdLineParser >>= work
