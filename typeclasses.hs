import Data.Semigroup
import Data.List.NonEmpty

ne1 = 1 :| [2, 3]
ne2 = "x" :| []

ne3 = sconcat $ "x" :| ["y", "z"]