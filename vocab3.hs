{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import System.Environment(getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ord(comparing, Down(..))

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab text = map buildEntry . group . sort $ ws
  where 
    ws = 
      map T.toCaseFold 
        . filter (not . T.null) 
        . map cleanWord 
        . T.words $ text
    buildEntry ws@(w:_) = (w, length ws)
    cleanWord = T.dropAround (not . isLetter)

allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab = 
  T.append "\nAll words:\n" 
    . T.unlines 
    . map fst $ vocab

wordsCount :: Vocabulary -> Int
wordsCount = sum . map snd

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport = T.append "\nWords count:\n" . T.pack .show . wordsCount   

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortBy . comparing $ Down . snd

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n = 
  T.append "\nFrequent words:\n"
  . T.unlines
  . map showEntry
  . take n
  . wordsByFrequency $ vocab 
    where showEntry (t, n) = T.append t . T.pack . (++) " - " . show $ n  

processTextFile :: FilePath -> Int -> IO()
processTextFile fName n = do
  text <- TIO.readFile fName
  let vocab = extractVocab text
  TIO.putStrLn . allWordsReport $ vocab
  TIO.putStrLn . wordsCountReport $ vocab
  TIO.putStrLn . frequentWordsReport vocab $ n

main :: IO()
main = do
  args <- getArgs
  case args of
    [fName, n] -> processTextFile fName (read n)
    _ -> putStrLn "Usage: vocab-builder filename number_of_frequent_words"
