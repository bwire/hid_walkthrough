{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Writer
import Data.Foldable

type SQL = T.Text

data ErrMsg = WrongFormat Int T.Text deriving Show

genInsert s1 s2 = "INSERT INTO items VALUES ('" <> s1 <> "', '" <> s2 <> "');\n"

processLine :: (Int, T.Text) -> Writer [ErrMsg] SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""

genSQL :: T.Text -> Writer [ErrMsg] SQL
genSQL txt = T.concat <$> traverse processLine (zip [1..] $ T.lines txt)


testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
  let (sql, errors) = runWriter (genSQL testData)
  TIO.putStrLn "SQL:"
  TIO.putStr sql
  TIO.putStrLn "Errors:"
  traverse_ print errors

main :: IO ()
main = testGenSQL