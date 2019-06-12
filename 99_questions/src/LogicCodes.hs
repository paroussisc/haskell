module LogicCodes  where

-- Question 46
and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool
or' _ True = True
or' True _ = True
or' _ _    = False

not' :: Bool -> Bool
not' True  = False
not' False = True

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' False True = True
xor' True False = True
xor' _ _        = False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (f x y)
                          | x <-  [True, False], y <-  [True, False]]

-- Skipping questions 47-48 since they're really meant for other languages
-- / I cba.

-- Question 49
gray :: (Num a, Eq a) => a -> [String]
gray 1 = ["0", "1"]
gray n = map ('0' :) prev ++ map ('1' :) (reverse prev)
    where prev = gray (n-1)

--  Skipping 50 for now
