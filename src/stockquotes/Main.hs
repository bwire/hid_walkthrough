module Main where

import Control.Monad (when)
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Csv (decodeByName)

import Params
import QuoteData
import Statistics
import StatReport
import Charts

-- temporary definition
data QuoteDataCollection

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fName params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, quotes)  -> generateReport params quotes

generateReport :: (Functor t, Foldable t) => Params -> t QuoteData -> IO ()
generateReport Params {..} quotes = do
  TIO.putStrLn $ statReport statInfo'
  when prices $ plotChart title quotes [Open, Close, High, Low] fname_prices
  when volumes $ plotChart title quotes [Volume] fname_volumes
  where 
    statInfo' = statInfo quotes
    withCompany pref = if company /= "" then pref ++ company else ""
    img_suffix = withCompany "_" ++ ".svg"
    fname_prices = "prices" ++ img_suffix
    fname_volumes = "volumes" ++ img_suffix
    title = "Historical quotes" ++ withCompany " for "

readQuotes :: FilePath -> IO QuoteDataCollection
readQuotes = undefined

main :: IO ()
main = cmdLineParser >>= work
