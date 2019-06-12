module LogicCodes  where

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
