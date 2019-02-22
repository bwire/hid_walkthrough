-- extracting vocabulary
import Data.Char
import Data.List
import System.Environment(getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = do
  [fName] <- getArgs
  text <- TIO.readFile fName
  let ws = map head 
            . group
            . sort
            . map T.toCaseFold
            . filter (not . T.null)  
            . map (T.dropAround $ not . isLetter) 
            . T.words $ text
  TIO.putStrLn . T.unwords $ ws
  print . length $ ws
