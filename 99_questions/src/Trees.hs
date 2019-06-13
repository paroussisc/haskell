module Trees where

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)
leaf x = Branch x Empty Empty

-- Question 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- cbalTree i,
                                right <- cbalTree (n - i - 1)]
              where (q, r) = (n-1) `quotRem` 2

mirror :: Tree a -> Tree a -> Bool
mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _                           = False

symmetric :: Tree a -> Bool
symmetric t = mirror t t

construct :: [Integer] -> Tree Integer
construct [] = Empty
construct (xs:x) =  Branch xs (construct lower) (construct upper)
    where lower = [z | z <- x, z < xs]
          upper = [z | z <- x, z > xs]

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- Question 59
hbalTree :: Char -> Int -> [Tree Char]
hbalTree x = map fst . hbalTree'
    where hbalTree' 0 = [(Empty, 0)]
          hbalTree' 1 = [(Branch x Empty Empty, 1)]
          hbalTree' n =  [(Branch x lb rb, h) | (lb,lh) <- t, (rb,rh) <- t
                                         , let h = 1 + max lh rh, h == n]
                            where t = hbalTree' (n-2) ++ hbalTree' (n-1)
-- Question 60

-- Question 61
countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r

-- Question 61A
leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r)         = leaves l ++ leaves r

-- Question 62
internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch v Empty Empty) = []
internals (Branch v l r)         = v : internals l ++ internals r
