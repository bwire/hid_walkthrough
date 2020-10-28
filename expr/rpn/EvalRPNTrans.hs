module EvalRPNTrans where

import Control.Monad.State
import Text.Read (readMaybe)
import Control.Applicative
 
type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push = modify' . (:)

pop :: EvalM Integer
pop = do
  xs <- get
  guard (not $ null xs)
  put (tail xs)
  pure (head xs)

pop' :: EvalM Integer
pop' = do
  (x:xs) <- get
  put xs
  pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  l <- length <$> get
  guard (l == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str = 
  case readMaybe str of
    Nothing -> empty
    Just n -> pure n

evalRPN :: String -> Maybe Integer
evalRPN expr = evalStateT eval' []
  where
    eval' :: EvalM Integer
    eval' = traverse step (words expr) >> oneElementOnStack >> pop

    step :: String -> EvalM ()
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step t = push . read $ t

    processTops :: (Integer -> Integer -> Integer) -> EvalM ()
    processTops op = flip op <$> pop <*> pop >>= push  