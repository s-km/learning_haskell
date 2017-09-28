-- helpers
lim :: Integral i => i -> i
lim x = floor . sqrt $ fromIntegral x

sieve :: [Integer]
sieve = filter isPrime (2:[3,5..])

firstFactor :: Integral i => i -> (i, i)
firstFactor m = f m [2..(lim m)]
  where
    f m [] = (1, m)
    f m (x:xs) = if m `mod` x == 0 then (x, m `quot` x) else f m xs

-- Problem 31 (**)
-- Determine whether a given integer number is prime.
isPrime :: Integral i => i -> Bool
isPrime 1 = False
isPrime x = all (\y -> x `mod` y /= 0) l
  where l = [2..(lim x)]

-- Problem 32 (**)
-- Determine the greatest common divisor of two positive integer numbers.
myGCD :: Integer -> Integer -> Integer
myGCD a b
  | b == 0    = abs a
  | otherwise = myGCD b (a `mod` b)

-- Problem 33 (*)
-- Determine whether two positive integer numbers are coprime.
coprime :: Integer -> Integer -> Bool
coprime a b = myGCD a b == 1

-- Problem 34 (**)
-- Calculate Euler's totient function phi(m).
totient :: Integer -> Integer
totient 1 = 1
totient m
  | isPrime m   = m - 1
  | coprime x y = totient x * totient y
  | otherwise = fromIntegral $ length [c | c <- [1..(m-1)], coprime c m]
  where (x, y) = firstFactor m

-- Problem 35 (**)
-- Determine the prime factors of a given positive integer.
primeFactors :: Integer -> [Integer]
primeFactors x = f x primes
  where
    f x _ | x<2  = []
    f x []       = x:[]
    f x l@(p:ps) =
      if x `mod` p == 0
        then p : f (x `quot` p) l
        else f x ps
    primes       = take (lim $ fromIntegral x) sieve

-- Problem 36 (**)
-- Determine the prime factors of a given positive Integer.
prime_factors_mult :: Integer -> [(Integer, Integer)]
prime_factors_mult x = f (primeFactors x)
  where
    f [] = []
    f l@(p:ps) = 
      let (a, b) = span (== p) l
      in (p, fromIntegral $ genericLength a) : f b

-- Problem 37 (**)
-- Calculate Euler's totient function phi(m) (improved).
totient' :: Integer -> Integer
totient' x = foldr phi' 1 pfm
  where
    phi' = (\x y -> let (p, m) = x in (p-1) * (p ^ (m-1)) * y)
    pfm  = (prime_factors_mult x)

-- Problem 39 (*)
-- A list of prime numbers.
primesR :: Integer -> Integer -> [Integer]
primesR i j = dropWhile (<= i) $ takeWhile (<= j) sieve

-- Problem 40 (**)
-- Goldbach's conjecture.
goldbach :: Integer -> (Integer, Integer)
goldbach n = f $ takeWhile (<= n) sieve 
  where
    f []     = error "n must be >2 and even"
    f l@(x:xs) =
      if (n-x) `elem` l
        then (x, (n-x))
        else f xs

-- Problem 41a (**)
-- Given a range, print all even numbers and their Goldbach composition
goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList i j = [gb | n <- [i..j], let gb = goldbach n, even n]

-- Problem 41b (**)
-- Given a range, print all even numbers and their Goldbach composition
-- where both primes are larger than some integer N.
goldbachList' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachList' i j n = filter (\x -> fst x >= n && snd x >= n) g
  where g = goldbachList i j
