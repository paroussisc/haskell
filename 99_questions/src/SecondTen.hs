module SecondTen  where

import           FirstTen

data ListItem a = Single a | Multiple Int a
    deriving (Show, Eq)

-- Question 11
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map transform . encode
    where
      transform(1, x) = Single x
      transform(n, x) = Multiple n x

-- Question 12
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap transform
    where
      transform :: ListItem a -> [a]
      transform(Single x)     = [x]
      transform(Multiple n x) = replicate n x
