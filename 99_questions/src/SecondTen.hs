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

-- Question 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli [x] 1    = [x]
repli [x] n    = x : repli [x] (n-1)
repli (x:xs) n = x : repli [x] (n-1) ++ (repli xs n)

-- Question 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = snd $ foldl helper (1, []) xs
    where
      helper (a,b) x = if a == n
                     then (1,b)
                     else (1+a,b++[x])

-- Question 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([],[])
split whole@(x:xs) n
    | n > 0 =  (x:ys, zs)
    | otherwise = ([], whole)
    where (ys, zs) = split xs (n-1)

-- Question 18
slice :: [a] -> Int -> Int -> [a]
slice xs m n = zs
    where (_, ys) = split xs (m-1)
          (zs, _) = split ys (n-m+1)

-- Question 19
rotate :: [a] -> Int -> [a]
rotate xs n = zs ++ ys
    where (ys, zs) = split xs j
          j | n < 0 = length xs + n
            | n < length(xs) = n
            | otherwise = n  - length xs
