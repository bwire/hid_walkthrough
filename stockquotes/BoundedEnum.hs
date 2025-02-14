module BoundedEnum (BoundedEnum(range)) where

import Prelude (Enum (enumFrom), Bounded (minBound))

class (Bounded a, Enum a) => BoundedEnum a where
  range :: [a]
  range = enumFrom minBound 
