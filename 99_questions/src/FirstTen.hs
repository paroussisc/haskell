module FirstTen  where

-- Question 1
myLast :: [a] -> a
myLast []     = error "Empty list, you swine!"
myLast [x]    = x
myLast (_:xs) = myLast(xs)

-- Question 2
myButLast :: [a] -> a
myButLast []     = error "Empty list, you swine!"
myButLast [x]    = error "Need list of at least size 2, idiot!"
myButLast [x, _] = x
myButLast (_:xs) = myButLast(xs)

-- Question 3
elementAt :: [a] -> Int -> a
elementAt list idx
    | idx > size || idx <= 0 = error "Idx out of bounds!"
    | idx == 1 = head list
    | (x:xs) <- list = elementAt xs (idx - 1)
    where size = length list

-- Question 4
myLength :: [a] -> Int
myLength []     = 0
myLength [x]    = 1
myLength (_:xs) = 1 + myLength(xs)

-- Question 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Question 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == myReverse list

data NestedList a = Elem a | List [NestedList a]

-- Question 7 - stolen from solutions
flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- Question 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x1] = [x1]
compress (x1:x2:xs)
    | x1 == x2 = compress (x2:xs)
    | otherwise = x1 : compress (x2:xs)

-- Question 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x1] = [[x1]]
pack (x:xs)
    | x `elem` (head (pack xs)) = (x:(head (pack xs))):(tail (pack xs))
    | otherwise = [x]:(pack xs)

-- Question 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (xs) = (enc . pack) xs
    where enc = foldr (\x acc -> (length x, head x) : acc) []
