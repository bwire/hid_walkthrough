import Safe

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

locByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locByName pnumbers locs name = lookup name pnumbers >>= flip lookup locs

doubleStrNumber :: (Num a, Read a) => String -> Maybe a
doubleStrNumber s = (*2) <$> readMay s

plusStrNumbers :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers s1 s2 = (+) <$> readMay s1 <*> readMay s2

main :: IO ()
main = undefined