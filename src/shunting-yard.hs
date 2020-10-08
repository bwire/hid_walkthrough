import Control.Monad.State

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
  (x:xs, output) <- get
  put (xs, output)
  return x

pop_ :: State MyState ()
pop_ = modify (\(x:xs, output) -> (xs, output))

push :: Token -> State MyState ()
push t = modify (\(state, output) -> (t:state, output))


main :: IO ()
main = undefined