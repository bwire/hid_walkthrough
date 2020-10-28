module Utils where

import Control.Monad.Reader
import Data.Foldable (traverse_)
import System.PosixCompat.Files
import System.Directory
import System.FilePath

import AppTypes
import AppRWST (MyApp)

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
  AppEnv { path, fileStatus }  <- ask
  liftIO $ fileStatus path

traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
    curPath <- asks path
    content <- liftIO $ listDirectory curPath
    traverse_ go content
  where
    go name = flip local app
              $ \env -> env {
                path = path env </> name,
                depth = depth env + 1
              }
   