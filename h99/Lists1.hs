-- Problem 1 (*)
-- Find the last element of a list.
myLast :: [a] -> a
myLast = head . reverse

-- Problem 2 (*)
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = last . init

-- Problem 3 (*)
-- Find the K'th element of a list.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "no Kth element of empty list my dude"
elementAt (x:_) 1 = x
elementAt (_:xs) k
  | k < 1     = error "k must be >=1 my dude"
  | otherwise = elementAt xs (k - 1)

-- Problem 4 (*)
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0

-- Problem 5 (*)
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (\x y -> y:x) []

-- Problem 6 (*)
-- Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (myReverse xs) == xs

-- Problem 7 (**)
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List xs) = concatMap myFlatten xs

-- Problem 8 (**)
-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (\y -> y == x) xs)

-- Problem 9 (**)
-- Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = fst split : pack (snd split)
  where split = span (== head xs) xs

-- Problem 10 (*)
-- Run-length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map (\x -> (myLength x, head x)) packed
  where packed = pack xs
