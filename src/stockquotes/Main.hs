module Main where

data Params
data QuoteData
data QuoteDataCollection
data StatInfo

work :: Params -> IO()
work = undefined

readData :: FilePath -> IO QuoteDataCollection
readData = undefined

statInfo :: QuoteDataCollection -> StatInfo
statInfo = undefined

--statReport :: StatInfo -> Text
--statReport = undefined

plotCharts :: Params -> QuoteDataCollection -> IO()
plotCharts = undefined

main :: IO ()
main = undefined
