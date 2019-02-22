import Data.Char
import Data.List
import System.Environment(getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

printAllWords :: Vocabulary -> IO()
printAllWords vocab = do
  putStrLn "All words:"
  TIO.putStrLn . T.unlines . map fst $ vocab

processTextFile :: FilePath -> IO()
processTextFile file = do
  text <- TIO.readFile file
  let vocab = extractVocab text
  printAllWords vocab

main :: IO()
main = do
  args <- getArgs
  case args of
    [file] -> processTextFile file
    _ -> putStrLn "Usage: vocab-builder filename"