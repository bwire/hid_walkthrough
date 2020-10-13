import Control.Monad.Writer
import Data.Monoid

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

gcdM :: (Integral a, Monad m) => (a -> a -> m()) -> (a -> a -> m a)
gcdM step a 0 = step a 0 >> pure a
gcdM step a b = step a b >> gcdM step b (a `mod` b)

gcd_logSteps :: (Integral a) => a -> a -> Writer [(a, a)] a
gcd_logSteps = gcdM (\a b -> tell [(a, b)])

gcd_logSteps' :: Integer -> Integer -> Writer [(Integer, Integer)] Integer
gcd_logSteps' a 0 = tell [(a, 0)] >> return a
gcd_logSteps' a b = tell [(a, b)] >> gcd_logSteps' b (a `mod` b)

gcd_countSteps :: (Integral a) => a -> a -> Writer (Sum Int) a
gcd_countSteps = gcdM (\_ _ -> tell . Sum $ 1)

gcd_countSteps' :: (Integral a) => a -> a -> Writer (Sum Int) a
gcd_countSteps' a b = mapWriter mapper (gcd_logSteps a b)
  where mapper (a, w) = (a, Sum . length $ w)

gcd_countSteps'' :: (Integral a) => a -> a -> Writer (Sum Int) a
gcd_countSteps'' = (mapWriter (Sum . length <$>) . ) . gcd_logSteps

main :: IO ()
main = do
  let log = execWriter (gcd_logSteps 27 36)
  putStrLn . show $ log