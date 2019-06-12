module FourthTen  where

import           Data.List (groupBy)

-- Question 31
isPrime :: (Eq a, Integral a) => a -> Bool
isPrime n     = gd == 1
    where  m = n `div` 2
           gd = length [y | y <- [1..m], n `rem` y == 0]

-- Question 32
gcd' :: (Eq a, Integral a) => a -> a -> a
gcd' 0 y     =  y
gcd' x y     =  helper (y `mod` x) x
    where  helper x y | x < 0  = helper (-x) y
                      | y < 0  = helper x (-y)
                      | x < y = gcd' x y
                      | otherwise = gcd' y x

-- Question 33
coprime :: (Eq a, Integral a) => a -> a -> Bool
coprime x y     =  (gcd' x y) == 1

-- Question 34
totient :: Int -> Int
totient   n  =  length $ filter (coprime n) [1..n]

-- Question 35
primeFactors :: Int -> [Int]
primeFactors   n  =  primeFactors' n 2
  where
    primeFactors' n f
      | f*f > n        = [n]
      | n `mod` f == 0 = f : primeFactors' (n `div` f) f
      | otherwise      = primeFactors' n (f + 1)

-- Question 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult   n  =   map (pairUp) $ groupBy (==)  $ primeFactors n
    where pairUp x = (head x, length x)

-- Question 37
totientImproved :: Int -> Int
totientImproved   1 = 1
totientImproved   n =  product [(p - 1)*p^(m-1) | (p,m) <- primeFactorsMult n]

-- Question 40
goldbach :: Int -> (Int, Int)
goldbach   n = head [(x, y) | x <- primes, y <- primes, x + y == n]
    where primes = filter (isPrime) [1..n]

-- Question 40
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList   l u = map goldbach [x | x <- [l..u], x `mod` 2 == 0]
