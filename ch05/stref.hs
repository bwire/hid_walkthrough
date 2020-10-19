import Control.Monad.ST
import Data.STRef

comp1 :: ST s (STRef s Int)
comp1 = newSTRef 42

comp2 :: STRef s Int -> (Int -> Int) -> ST s ()
comp2 = modifySTRef'

comp3 :: STRef s Int -> ST s Int
comp3 ref = readSTRef ref

main :: IO ()
main = print $ runST (comp1 >>= \ref -> flip comp2 (*2) ref >> comp3 ref)