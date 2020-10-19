import Control.Monad.ST
import Data.STRef
import Data.Foldable (traverse_)
import Control.Monad (when)

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
    c <- newSTRef 0
    traverse_ (\x -> when (x == 0) $ inc c) xs
    readSTRef c
  where 
    inc c = modifySTRef' c (+1)

main :: IO ()
main = print $ countZerosST [1,0,4,0,0,5]