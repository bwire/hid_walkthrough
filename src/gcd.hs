import Control.Monad.Writer

gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

gcd_CountSteps :: Integer -> Integer -> Writer (Sum Int) Integer
gcd_CountSteps a 0 = tell (Sum 1) >> return a
gcd_CountSteps a b = tell (Sum 1) >> gcd_CountSteps b (a `mod` b)

gcd_logSteps :: Integer -> Integer -> Writer [(Integer, Integer)] Integer
gcd_logSteps a 0 = tell [(a, 0)] >> return a
gcd_logSteps a b = tell [(a, b)] >> gcd_logSteps b (a `mod` b)

gcd_CountSteps' :: Integer -> Integer -> Writer (Sum Int) Integer
gcd_CountSteps' = (mapWriter (Sum . length <$>) .) . gcd_logSteps

main :: IO ()
main = do
  let log = execWriter (gcd_logSteps 27 36)
  putStrLn . show $ log