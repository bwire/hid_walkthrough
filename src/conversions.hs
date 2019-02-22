import Control.Applicative

circle :: Double -> Double
circle r = pi * r * r

-- more general
circle' :: Floating a => a -> a
circle' r = pi * r * r

-- since we can use a Complex value as a radius we need other restriction
circle'' :: (Real a, Floating b) => a -> b
circle'' r = pi * realToFrac (r * r )

xs :: [Int]
xs = [1,2,3,4,5]

avg :: [Int] -> Double
avg = (/) <$> fromIntegral . sum <*> fromIntegral . length