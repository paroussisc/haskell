module FourthTen  where

  -- Question 31
  isPrime :: (Eq a, Integral a) => a -> Bool
  isPrime n     = gd == 2
      where gd = length [y | y <- [1..n], n `rem` y == 0]
