module Main where

import Data.Text

import Params
import QuoteData

-- temporary definition
data QuoteDataCollection
data StatInfo

work :: Params -> IO ()
work = undefined

readQuotes :: FilePath -> IO QuoteDataCollection
readQuotes = undefined

statInfo :: QuoteDataCollection -> StatInfo
statInfo = undefined

statReport :: StatInfo -> Text
statReport = undefined

plotCharts :: Params -> QuoteDataCollection -> IO ()
plotCharts = undefined

main :: IO ()
main = undefined
