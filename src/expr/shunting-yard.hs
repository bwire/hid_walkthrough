import Control.Monad.State

-- Implementation of the Shunting-yard algorithm
data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type MyState = (Stack, Output)

isEmpty :: State MyState Bool
isEmpty = null <$> gets fst

notEmpty :: State MyState Bool
notEmpty = not <$> isEmpty

top :: State MyState Token
top = head <$> gets fst

pop :: State MyState Token
pop = do
  (t : ts, s) <- get
  put (ts, s)
  return t

pop_ :: State MyState ()
pop_ = modify $ \(s, es) -> (tail s, es)

push :: Token -> State MyState ()
push t = modify $ \(s, es) -> (t:s, es)

whileNotEmptyAnd :: (Token -> Bool) -> State MyState () -> State MyState ()
whileNotEmptyAnd pred m = go
  where 
    go = do
      b1 <- notEmpty
      when b1 $ do
        b2 <- pred <$> top
        when b2 (m >> go) 


main :: IO ()
main = undefined