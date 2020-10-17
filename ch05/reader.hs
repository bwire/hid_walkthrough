import Control.Monad.Reader

data Config = Config {
  flag1 :: Bool,
  flag2 :: Bool
}

type ConfigM = Reader Config

work :: ConfigM ()
work = do
  f1

f1 :: ConfigM ()
f1 = f11

f11 :: ConfigM ()
f11 = do
  c <- ask
  pure ()

main :: IO ()
main = do
  let c = Config True False
  return . runReader work $ c
  print "OK"

