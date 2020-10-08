import System.Random
import Control.Monad.State
import Data.List (group, sort)

data Weapons = Rock | Paper | Scissors
  deriving (Eq, Enum, Bounded)

data Winner = First | Second | Draw
  deriving (Eq, Ord, Show)

instance Random Weapons where
  randomR (a, b) g = 
    case randomR (fromEnum a, fromEnum b) g of 
      (r, g1) -> (toEnum r, g1)
  random = randomR (minBound, maxBound) 

winner :: (Weapons, Weapons) -> Winner
winner (Rock, Paper) = Second
winner (Paper, Scissors) = Second
winner (Scissors, Rock) = Second
winner (w1, w2) = if (w1 == w2) then Draw else First

randomWeapon :: State StdGen Weapons
randomWeapon = state random

gameRound :: State StdGen (Weapons, Weapons)
gameRound = (,) <$> randomWeapon <*> randomWeapon

game :: Int -> State StdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
  where counts = map ((,) <$> head <*> length) . group . sort

main :: IO ()
main = do
  g <- newStdGen
  let r = evalState (game 10) g 
  print r