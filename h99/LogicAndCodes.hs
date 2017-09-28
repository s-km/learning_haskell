import Data.List (sortBy)
import Data.Ord (comparing)

data Huffman =
  Branch { freq :: Int, left :: Huffman, right :: Huffman } |
  Leaf { char :: Char, freq :: Int }
  deriving (Eq, Ord, Show)

-- Problem 46a (**)
-- Define predicates not', and', or', nor', xor', impl', and equ'.
not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' a b = and' (not' $ and' a b) (or' a b)

impl' :: Bool -> Bool -> Bool
impl' a b = or' (not' a) b

equ' :: Bool -> Bool -> Bool
equ' a b = xor' (not' a) b

-- Problem 46b (**)
-- Write a predicate table which prints the truth table
-- of a given logical expression in two variables.
table :: (Bool -> Bool -> Bool) -> IO ()
table p = mapM_ (putStrLn . show) truthTable
  where
    truthTable = 
      [ (a, b, p a b) |
         a <- [True, False],
         b <- [True, False] ]

-- Problem 47 (*)
-- Redefine and', or', etc. as operators, using Java precedence.
infixl 7 `equ'`
infixl 6 `and'`
infixl 5 `xor'`
infixl 4 `or'`

-- Problem 48 (**)
-- Generalize table in such a way that a predicate
-- may contain any number of logical variables.
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n p = mapM_ (putStrLn . show) truthTable
  where
    truthTable = map (\x -> x ++ [p x]) combinations
    combinations = mapM (const [True, False]) [1..n]

-- Problem 49 (**)
-- Gray codes.
gray :: Int -> [String]
gray 0 = [""]
gray n = foldr (\x y -> ("0" ++ x):("1" ++ x) : y) [] (gray (n-1))

-- Problem 50 (***)
-- Huffman codes.
huffman :: [(Char, Int)] -> [(Char, String)]
huffman [] = []
huffman freqList = sortBy (comparing fst) $ encode tree []
  where
    encode (Branch _ l r) encStr = 
      (encode l ('0' : encStr)) ++ (encode r ('1' : encStr))
    encode (Leaf c _) encStr     = [(c, encStr)]
    tree = mkHuffman $ map (uncurry Leaf) freqList

mkHuffman :: [Huffman] -> Huffman
mkHuffman [x] = x
mkHuffman l = f $ sortBy (comparing freq) l
  where
    f (x:y:ys) = mkHuffman $ (Branch (freq x + freq y) x y) : ys
