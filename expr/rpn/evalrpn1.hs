import Control.Monad.State

type Stack = [Integer]
type EvalM = State Stack

push :: Integer -> EvalM ()
push = modify' . (:)

pop :: EvalM Integer
pop = state $ \(x:xs) -> (x, xs)

evalRPN :: String -> Integer
evalRPN expr = evalState eval' []
  where
    eval' :: EvalM Integer
    eval' = traverse step (words expr) >> pop

    step :: String -> EvalM ()
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step t = push . read $ t

    processTops :: (Integer -> Integer -> Integer) -> EvalM ()
    processTops op = flip op <$> pop <*> pop >>= push  

main :: IO ()
main = print $ evalRPN "3 2 +"