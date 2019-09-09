{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable (traverse_)

data ErrorMsg = WrongFormat Int T.Text deriving Show

type SQL = T.Text

genInsert :: T.Text -> T.Text -> T.Text
genInsert sf sv = "INSERT INTO items VALUES ('" <> sf <> "', '" <> sv <> "');\n"   

processOneLine :: (Int, T.Text) -> Writer [ErrorMsg] SQL
processOneLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processOneLine (i, s) = writer (T.empty, [WrongFormat i s]) 

genSQL :: T.Text -> Writer [ErrorMsg] SQL
genSQL t = T.concat <$> traverse processOneLine (zip [1..] $ T.lines t)

main :: IO ()
main = do
  let testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"
  let (sql, errors) = runWriter (genSQL testData)
  TIO.putStrLn "SQL:"
  TIO.putStr sql
  TIO.putStrLn "Errors:"
  traverse_ print errors