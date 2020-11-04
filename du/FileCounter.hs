module FileCounter (fileCount) where

import System.Directory.Extra (listFiles)
import App
import Utils

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp = maybe True (`isExtensionOf` fp) (extension cfg)

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    traverseDirectoryWith fileCount
    files <- liftIO $ listFiles path
    tell [(path, length $ filter (checkExtension cfg) files)]

