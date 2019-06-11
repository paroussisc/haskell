module FourthTen  where

  -- Question 31
  isPrime :: (Eq a, Integral a) => a -> Bool
  isPrime n     = gd == 1
      where  m = n `div` 2
             gd = length [y | y <- [1..m], n `rem` y == 0]
