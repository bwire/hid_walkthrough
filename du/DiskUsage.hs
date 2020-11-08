module DiskUsage (diskUsage) where

import App
import Utils
data DuEntryAction = 
    TraverseDir { dirPath :: FilePath, requireReporting :: Bool }
  | RecordFileSize { fSize :: FileOffset }
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv {..} fs
      | isDirectory fs = TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path = RecordFileSize (fileSize fs)
      | otherwise = None
    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit <- get
        tell [(dirPath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize { fSize } = modify (+fSize)
    processEntry None = pure ()
