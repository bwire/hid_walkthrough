import Data.IORef
import Text.Read (readMaybe)

sumNumbers :: IO Int
sumNumbers = do
    s <- newIORef 0
    go s 
  where
    go :: IORef Int -> IO Int
    go acc = readNumber >>= processNumber acc

    readNumber :: IO (Maybe Int)
    readNumber = do
      putStr "Put integer number (not a number to finish): "
      readMaybe <$> getLine

    processNumber :: IORef Int -> (Maybe Int) -> IO Int
    processNumber acc Nothing = readIORef acc
    processNumber acc (Just n) = modifyIORef' acc (+n) >> go acc

main :: IO ()
main = do 
  s <- sumNumbers
  print s
