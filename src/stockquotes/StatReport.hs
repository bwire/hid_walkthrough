module StatReport () where

import Fmt
import Data.Text (Text)
import Data.Fixed (showFixed)
import QuoteData (QField(..))
import Statistics (Statistic(..), StatEntry(..), StatQFieldData)

instance Buildable Statistic where
  build Mean = "Mean"
  build Min = "Minimum"
  build Max = "Maximum"
  build Days = "Days between Min/Max"

showStatEntryValue :: StatEntry -> String
showStatEntryValue StatEntry {..} = showFixed (removeTrailings stat qfield) value where
  removeTrailings Days _ = True
  removeTrailings Min Volume = True
  removeTrailings Max Volume = True
  removeTrailings _ _ = False

instance Buildable StatEntry where
  build se@StatEntry {..} = "" +|stat|+ ": " +|showStatEntryValue se|+ ""

instance Buildable StatQFieldData where
  build (qf, stats) = nameF ("Statistic for " +||qf||+ ": ") $ unlinesF stats 

statReport :: StatInfo -> Text
statReport = fmt . unlinesF
