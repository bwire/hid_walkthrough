import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.Directory.Extra (listContents, doesDirectoryExist)
import Control.Monad.Extra (whenM, ifM, zipWithM_)
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)

fileCount :: FilePath -> IO Int 
fileCount fpath = do
    counter <- newIORef 0
    whenM (doesDirectoryExist fpath) $ go counter fpath
    readIORef counter
  where
    go :: IORef Int -> FilePath -> IO ()
    go cnt fp = listContents fp >>= traverse_ (processEntry cnt)

    processEntry :: IORef Int -> FilePath -> IO ()
    processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)

    inc :: IORef Int -> IO ()
    inc cnt = modifyIORef' cnt (+1)

main :: IO ()
main = do
    args <- getArgs
    xs <- traverse fileCount args
    zipWithM_ printEntry args xs
  where
    printEntry :: FilePath -> Int -> IO ()
    printEntry fp n = putStrLn (show n ++ "\t" ++ fp)
