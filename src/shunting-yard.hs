import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Char (isSpace, isDigit)
import Data.List (groupBy)

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)
  deriving (Show)

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type SYState = (Stack, Output)

isEmpty :: State SYState Bool
isEmpty = null <$> gets fst

notEmpty :: State SYState Bool
notEmpty = not <$> isEmpty

top :: State SYState Token
top = gets (head . fst)

pop :: State SYState Token
pop = do
  (s, o) <- get
  put (tail s, o)
  return (head s)

pop_ :: State SYState ()
pop_ = modify (\(s, o) -> (tail s, o))

push :: Token -> State SYState ()
push t = modify (\(s, o) -> (t:s, o)) 

whileNotEmptyAnd :: (Token -> Bool) -> State SYState () -> State SYState ()
whileNotEmptyAnd pred m = go
  where
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- pred <$> top
        when b2 (m >> go)

output :: Token -> State SYState ()
output t = modify (builder t <$>) where
  builder "+" (e1 : e2 : es) = Add e1 e2 : es
  builder "*" (e1 : e2 : es) = Mult e1 e2 : es 
  builder n es = Lit (read n) : es

isOp :: Token -> Bool
isOp "+" = True
isOp "*" = True
isOp _ = False

precedence :: Token -> Int
precedence "+" = 1
precedence "*" = 2
precedence _ = 0

precGTE :: Token -> Token -> Bool
t1 `precGTE` t2 = precedence t1 >= precedence t2

convertToExpt :: String -> Expr Integer
convertToExpt s = head . snd . execState shuntingYard $ ([], [])
  where
    shuntingYard :: State SYState ()
    shuntingYard = traverse_ processToken tokens >> processRest

    tokens :: [String]
    tokens = reverse . tokenize $ s

    tokenize :: Token -> [Token]
    tokenize = groupBy (\a b -> isDigit a && isDigit b) . filter (not . isSpace)

    processRest :: State SYState ()
    processRest = transferWhile (const True)

    transferWhile :: (Token -> Bool) -> State SYState ()
    transferWhile p = whileNotEmptyAnd p (pop >>= output)

    processToken :: Token -> State SYState ()
    processToken ")" = push ")"
    processToken "(" = transferWhile (/= ")") >> pop_
    processToken t 
      | isOp t = transferWhile (`precGTE` t) >> push t
      | otherwise = output t

main :: IO ()
main = do
  let expr = convertToExpt "(2 + 135) * (5 + 3)"
  print expr 