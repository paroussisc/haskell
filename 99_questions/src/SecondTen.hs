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

encodePair :: Eq a => [a] -> [(Int,a)]
encodePair = foldr acca []
    where
      acca x [] = [(1,x)]
      acca x (y@(c,d):ys)
        | x == d    = (1+c,x):ys
        | otherwise = (1,x):y:ys

-- Question 13
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect = map transform . encodePair
    where
      transform(1,x) = Single x
      transform(n,x) = Multiple n x

-- Question 14
dupli :: (Eq a) => [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:(dupli xs)
