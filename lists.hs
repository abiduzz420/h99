import Data.List

-- 01 last element in the list
myLast :: [a] -> a
myLast [] = error "List is empty"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

-- 02 last but one element in the list
myButLast :: [a] -> a
myButLast [] = error "List empty"
myButLast [x] = error "Only one element"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs

-- 03 Kth element in the list. First element is k=1
elementAt :: Int -> [a] -> a
elementAt k list
  | length list < k = error "k value too big"
  | otherwise = (last . take k) list

elementAt' :: Int -> [a] -> a
elementAt' k list = (!!) list (k-1)

elementAt'' :: Int -> [a] -> a
elementAt'' k list = (head . drop (k-1)) list

-- 04 Number of elements in the list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' list = sum [1 | x <- list]

-- 05 Reverse a list
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse list = (last list) : myReverse (init list)
 
