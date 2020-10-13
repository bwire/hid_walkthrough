{-# LANGUAGE InstanceSigs #-}

import System.Random
import Control.Monad.State
import Data.List (group, sort)

data Weapon = Rock | Paper | Scissors
  deriving (Eq, Bounded, Enum, Ord, Show)

instance Random Weapon where
  randomR :: RandomGen g => (Weapon, Weapon) -> g -> (Weapon, g) 
  randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
    (v, g') -> (toEnum v, g')
  
  random :: RandomGen g => g -> (Weapon, g)
  random = randomR (minBound, maxBound)


data Winner = First | Second | Draw
  deriving (Show, Eq, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
  | w1 == w2 = Draw
  | otherwise = Second

randomWeapon :: State StdGen Weapon
randomWeapon = state random

gameRound :: State StdGen (Weapon, Weapon)
gameRound = do
  w1 <- randomWeapon
  w2 <- randomWeapon
  return (w1, w2)

gameRound' :: State StdGen (Weapon, Weapon)
gameRound' = (,) <$> randomWeapon <*> randomWeapon

-- game :: Int -> State StdGen [[Weapon]]
game n = counts <$> replicateM n (winner <$> gameRound) 
  where 
    counts = fmap hl . group . sort
    hl = (,) <$> head <*> length

main :: IO ()
main = do
  g <- newStdGen
  let r = evalState (game 30) g
  print r