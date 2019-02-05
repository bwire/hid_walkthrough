{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.List (nub, sort)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

class (Enum a, Bounded a) => BoundedEnum a where
  range :: [a]
  range = [minBound .. maxBound]

data Direction = North | South | West |East deriving (Eq, Enum, Bounded, Show, CyclicEnum)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, BoundedEnum)
  
orient :: Turn -> Direction -> Direction
orient TNone = id
orient TLeft = cpred
orient TRight = csucc
orient TAround = cpred . cpred

findTurn :: Direction -> Direction -> Turn
findTurn ds dd = head . filter (\t -> orient t ds == dd) $ [TNone, TLeft, TRight, TAround] 

-- more generalized approach
findTurn' :: Direction -> Direction -> Turn
findTurn' ds dd = head . filter (\t -> orient t ds == dd) $ range

-- just for the demonstration of StandaloneDeriving
deriving instance BoundedEnum Direction
deriving instance Ord Turn

-- test scenario
test :: Bool
test = (sort . nub  $ [findTurn d1 d2 | d1 <- range, d2 <- range]) == range

