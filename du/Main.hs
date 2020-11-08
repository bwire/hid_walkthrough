module Main where

import App
import TextShow
import Data.Text.IO as TIO
import DirTree
import FileCounter
import DiskUsage

buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries = 
  unlinesB $ title : map entryBuilder entries

tabEntryBuilder :: (TextShow s) => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

treeEntityBuilder :: (FilePath, Int) -> Builder
treeEntityBuilder (fp, n) = fromString indent <> fromString fp
  where indent = replicate (2 * n) ' '

work :: AppConfig -> IO ()
work config = do
  (_, dirs) <- runMyApp dirTree config ()
  (_, counters) <- runMyApp fileCount config ()
  (_, usages) <- runMyApp diskUsage config (0 :: FileOffset)
  let report = toText $
               buildEntries "Directory tree:" treeEntityBuilder dirs
               <> buildEntries "File counter:" tabEntryBuilder counters
               <> buildEntries "Space usage:" tabEntryBuilder usages
  TIO.putStr report
  
main :: IO ()
main = undefined