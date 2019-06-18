module Misc where

queens :: Int -> [[Int]]
queens n = filter test (generate n)
    where generate 0 = [[]]
          generate k = [q : qs | q <- [1..n], qs <- generate (k-1)]
          test []     = True
          test (q:qs) = isOK q qs && test qs
          isOK q qs = not (q `elem` qs || isDiag q qs)
          isDiag b bs = any (\(col_diff,other_row) -> abs (b - other_row) == (col_diff - 1)) $ zip [2..] bs
