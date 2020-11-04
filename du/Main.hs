module Main where

import TextShow

treeEntityBuilder :: (FilePath, Int) -> Builder
treeEntityBuilder (fp, n) = fromString indent <> fromString fp
  where indent = replicate (2 * n) ' '

main :: IO ()
main = undefined