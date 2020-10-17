import Control.Monad.State
import Data.Foldable

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (n + s)

addItem' :: Integer -> State Integer ()
addItem' n = modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList = traverse_ addItem' 

main :: IO ()
main = do
  let v = execState (sumList [3,4,5]) 0
  putStrLn . show $ v