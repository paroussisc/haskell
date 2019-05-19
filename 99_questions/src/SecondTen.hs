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
