module Multi where

  data Tree a = Node a [Tree a]
        deriving (Eq, Show)

  tree1 = Node 'a' []

  tree2 = Node 'a' [Node 'b' []]

  tree3 = Node 'a' [Node 'b' [Node 'c' []]]

  tree4 = Node 'b' [Node 'd' [], Node 'e' []]

  tree5 = Node 'a' [
                  Node 'f' [Node 'g' []],
                  Node 'c' [],
                  Node 'b' [Node 'd' [], Node 'e' []]
                  ]

  -- Question 70C
  nnodes :: Tree a -> Int
  nnodes (Node _ ts) = 1  + sum (map nnodes ts)

  -- Question 71 - shamelessly stolen
  ipl :: Tree a -> Int
  ipl = ipl' 0
    where ipl' d (Node _ ts) = d + sum (map (ipl' (d+1)) ts)

  -- Question 72
  bottom_up :: Tree a -> [a]
  bottom_up (Node v ts) = concatMap (bottom_up) ts ++ [v]
