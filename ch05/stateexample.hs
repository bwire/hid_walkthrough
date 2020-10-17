import Data.Foldable (traverse_);
import Control.Monad.State

addItem :: Integer -> State Integer ()
addItem n = do
  modify' (+n)

sumList :: [Integer] -> State Integer ()
sumList = traverse_ addItem 

main :: IO ()
main = putStrLn . show . execState (sumList [1..100]) $ 0