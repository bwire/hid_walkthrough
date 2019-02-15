{-# LANGUAGE OverloadedStrings #-}

import Data.String

data Person = Person String (Maybe Int) deriving Show

instance IsString Person where
  fromString name = Person name Nothing 

spj :: Person
spj = "Simon Peyton Jones"