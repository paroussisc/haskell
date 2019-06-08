module ThirdTen  where

import           Control.Monad (replicateM)
import           Data.Function
import           Data.List     (groupBy)
import           SecondTen
import           System.Random

data ListItem a = Single a | Multiple Int a
    deriving (Show, Eq)

-- Question 21
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = ys ++ c:zs
    where
      (ys, zs) = SecondTen.split xs n

-- Question 22
range :: Int -> Int -> [Int]
range a b
    | a < b = a:(range (a+1) b)
    | a == b = [b]
    | otherwise = error "First argument bigger than second :O"

-- Question 23 - stole this :(
-- also no test
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]

-- Question 24 - no test
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = rndSelect [1..m] n

-- Question 25 - no test
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect xs (length xs)

-- Question 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]

-- Question 27
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] xs = [[]]
group (g:gs) xs = concatMap helper $ combination g xs
          where helper (as, bs) = map (as:) (group gs bs)

-- Question 28
lsort :: (Eq a) => [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = smaller ++ [x] ++ bigger
     where n = length x
           smaller = lsort [z | z <- xs, (length z) < n]
           bigger = lsort [z |  z <- xs, (length z) >= n]


lfsort :: (Eq a) =>[[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort
