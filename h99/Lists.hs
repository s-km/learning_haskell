import System.Random

data NestedList a = Elem a | List [NestedList a]

data Encoded a = Single a | Multiple Int a
  deriving (Show)

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
elementAt l k = last $ take k l

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
isPalindrome l = (myReverse l) == l

-- Problem 7 (**)
-- Flatten a nested list structure.
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List l) = concatMap myFlatten l

-- Problem 8 (**)
-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (== x) xs)

-- Problem 9 (**)
-- Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l = dup : pack diff
  where (dup, diff) = span (== head l) l

-- Problem 10 (*)
-- Run-length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode l = map (\x -> (myLength x, head x)) packed
  where packed = pack l

-- Problem 11 (*)
-- Modified run-length encoding.
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map modify . encode
  where 
    modify (1, x) = Single x
    modify (n, x) = Multiple n x

-- Problem 12 (**)
-- Decode a run-length encoded list.
decodeModified :: Eq a => [Encoded a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple n x:xs) = replicate n x ++ decodeModified xs

-- Problem 13 (**)
-- Run-length encoding of a list (direct solution)
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect l
  | len > 1   = (Multiple len x) : (encodeDirect $ diff)
  | otherwise = (Single x) : (encodeDirect $ diff)
  where
    (dup, diff) = span (== head l) l
    (len, x)    = (length dup, head dup)

-- Problem 14 (*)
-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

-- Problem 15 (**)
-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli l n = concat $ (take n . repeat) l

-- Problem 16 (**)
-- Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery l n = keep ++ dropEvery (tail rest) n
  where (keep, rest) = splitAt (n - 1) l

-- Problem 17 (*)
-- Split a list into two parts.
split :: [a] -> Int -> ([a], [a])
split l n = f l [] n
  where
    f r l 0      = (l, r)
    f (x:xs) l n = f xs (l ++ [x]) (n - 1) 

-- Problem 18 (**)
-- Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice l start end = take (end - start + 1) . drop (start - 1) $ l

-- Problem 19 (**)
-- Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate as n = r ++ l
  where
    len    = length as
    (l, r) = splitAt ((len + n) `mod` len) $ as

-- Problem 20 (*)
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs)
  | n > 0      = (l, x:r)
  | otherwise  = error "K must be >0"
  where (l, r) = removeAt (n-1) xs

-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt y l 1 = y:l
insertAt y (x:xs) n
  | n > 0     = x : insertAt y xs (n-1)
  | otherwise = error "index starts at 1 for some reason :^)"

-- Problem 22
-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range start end = [start..end]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select l n = getStdRandom $ rnd_select' l n

rnd_select' :: RandomGen r => [a] -> Int -> r -> ([a], r)
rnd_select' [] _ rng = ([], rng)
rnd_select' _ 0 rng = ([], rng)
rnd_select' l n rng =
  let
    (pos, rng') = randomR (1, length l) rng
    (x, xs) = removeAt pos l
    (ys, rng'') = rnd_select' xs (n-1) rng'
  in (x : ys, rng'')

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M
diff_select :: Int -> Int -> IO [Int]
diff_select n m
  | n <= m    = rnd_select [1..m] n 
  | otherwise = error "set too small"

-- Problem 25
-- Generate a random permutation of the elements of a list.
rnd_permu :: [a] -> IO [a]
rnd_permu l = rnd_select l (length l)

-- Problem 26 (**)
-- Generate the combinations of K distinct objects
-- chosen from the N elements of a list.
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n l
  | n > 0     = 
    [ l !! x : ys |
      x <- [0..len],
      ys <- combinations (n-1) (rest x) ]
  | otherwise = error "K must be >0"
  where
    len    = length l - 1
    rest n = drop (n + 1) l

-- Problem 27
-- Group the elements of a set into disjoint subsets.
group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group _ [] = [[]]
group sub@(s:ss) l
  | sum sub <= length l = 
      [ x : y | 
        x <- combinations s l,
        y <- group ss (remove x) ]
  | otherwise = error "subsets gotta add up to set length"
  where
    remove y = filter (\x -> not (x `elem` y)) l

-- Problem 28 a
-- Sorting a list of lists according to length of sublists
lsort :: Ord a => [[a]] -> [[a]]
lsort (x:xs) =
  let
    shorter = lsort [y | y <- xs, length y <= length x]
    longer  = lsort [z | z <- xs, length z > length x]
  in shorter ++ [x] ++ longer
lsort _ = []

-- Problem 28 b
-- Sorting a list of lists according to rarity of length
lfsort :: Ord a => [[a]] -> [[a]]
lfsort [[]] = []
lfsort xs = concat $ lsort (f xs [])
  where
    f (x:xs) ls = 
      if (not $ length x `elem` ls)
      then (collect x xs) : f xs (length x:ls)
      else f xs ls
    f [] _ = []
    collect y ys = y:(filter (\x -> length x == length y) ys)