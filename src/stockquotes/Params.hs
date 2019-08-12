module Params (Params(..), cmdLineParser) where

import Data.Semigroup ((<>))
import Options.Applicative

data Params = Params {
    fname :: FilePath
  , company :: String
  , prices :: Bool
  , volumes :: Bool
  , html :: Bool
  , noText :: Bool
}

mkParams :: Parser Params
mkParams = 
  Params 
    <$> strArgument (metavar "FILE" <> help "CSV file name")
    <*> strOption (long "Company" <> short 'c' <> help "stock company's name" <> value "")
    <*> switch (long "prices" <> short 'p' <> help "create file with prices chart")
    <*> switch (long "volumes" <> short 'v' <> help "create file with volumes chart")
    <*> switch (long "html" <> help "generate html report")
    <*> switch (long "notext" <> short 'n' <> help "do not print a statistic report") 

cmdLineParser :: IO Params
cmdLineParser = execParser opts where
  opts = info (mkParams <**> helper)  
              (fullDesc <> progDesc "Stock quotes data processing")