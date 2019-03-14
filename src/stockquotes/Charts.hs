module Charts (plotCharts) where

import Data.Foldable (traverse_, toList)
import Graphics.Rendering.Chart.Easy (plot, line, (.=), layout_title)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile, loadSansSerifFonts, FileOptions(..), FileFormat(SVG))

import QuoteData

plotCharts :: (Functor t, Foldable t) => String -> t QuoteData -> [QField] -> FilePath -> IO ()
plotCharts title quotes qfs fName = toFile fileOptions fName $ do
    layout_title .= title
    traverse_ plotLine qfs
  where
    fileOptions = FileOptions (800, 600) SVG loadSansSerifFonts
    plotLine qf = plot $ line (show qf) [toList $ fmap (qf2pd qf) quotes]
    qf2pd qf q = (day q, (realToFrac . field2fun qf) q :: Double)