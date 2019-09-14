import Language.Haskell.Interpreter
import Data.Foldable(traverse_)
import Text.Show

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

instance (Show a) => Show (Expr a) where
  showsPrec _ (Lit a) = shows a 
  showsPrec p (Add e1 e2) = showParen (p > precAdd) $
    showsPrec precAdd e1
      . showString "+"
      . showsPrec precAdd e2
    where precAdd = 5
  showsPrec p (Mult e1 e2) = showParen (p > precMul) $
    showsPrec precMul e1
      . showString "*"
      . showsPrec precMul e2
    where precMul = 6 
 
myEval :: Num a => Expr a -> a
myEval (Lit a) = a
myEval (Add e1 e2) = myEval e1 + myEval e2
myEval (Mult e1 e2) = myEval e1 * myEval e2

testexpr :: Expr Int -> IO ()
testexpr e = do
  let e_str = show e
      e_val = myEval e
  putStr $ e_str ++ " = " ++ show e_val ++ " "
  r <- runInterpreter $ setImports["Prelude"] >> eval e_str
  case r of 
    Right r' -> if read r' == e_val
                then putStrLn "ok"
                else putStrLn "eval error"
    _ -> putStrLn "interpreter error"  
  putStrLn ""

main :: IO ()
main = traverse_ testexpr exprs

exprs :: [Expr Int]
exprs = [
  Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5),
  Add (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2))
                           (Add (Lit 2) (Mult (Lit 2) (Add (Lit 1) (Lit 2))))))
      (Add (Lit 1) (Mult (Lit 3) (Lit 2))),
  Add (Add (Add (Lit 1) (Lit 2)) (Add (Lit 1) (Lit 2)))
      (Add (Add (Lit 1) (Lit 2)) (Add (Lit 1) (Lit 2))),
  Mult (Mult (Mult (Lit 1) (Lit 2)) (Mult (Lit 1) (Lit 2)))
       (Mult (Mult (Lit 1) (Lit 2)) (Mult (Lit 1) (Lit 2))),
  Add (Mult (Lit 1) (Mult (Add (Lit 1) (Lit 2))
                     (Mult (Lit 2) (Add (Lit 2) (Mult (Lit 1) (Lit 2))))))
      (Add (Lit 1) (Add (Lit 3) (Lit 2)))
          ]