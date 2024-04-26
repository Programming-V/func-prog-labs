module PrimeSum where

-- https://www.hackerrank.com/challenges/prime-sum/problem


-- Using the Sieve of Eratosthenes

removeMul :: Int -> [Int] -> [Int]
removeMul n xs = [x | x <- xs, x `mod` n /= 0]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (removeMul x xs)

primes :: [Int]
primes = sieve [2..]

sumPrimes :: Int -> Int -> Bool
sumPrimes n k = sumPrimes'' n k primes
  where
    primes = sieve [2..n]

{- 
    This function solves the easy problems such as, 

    ghci> primeSum (10, 2)
        "Yes"
    ghci> primeSum (1, 6)
        "No"

    but if we write an input like this, we have to 
    stop it due to it takes too much time and may not be able to solve it.

    ghci> primeSum (100000000, 6)
        "^CInterrupted.
-}
sumPrimes' :: Int -> Int -> [Int] -> Bool
sumPrimes' n 0 _ = n == 0 
sumPrimes' 0 _ _ = True
sumPrimes' _ _ [] = False 
sumPrimes' n k (p:ps) = p <= n && (sumPrimes' (n - p) (k - 1) (p:ps) || sumPrimes' n k ps)

{-
    In the other hand, this function looks to solve
    big inputs as the previous one or other larger ones,

    ghci> primeSum (100000000, 6)
        "Yes"

    Also, this function solves the easy inputs.

    ghci> primeSum (10, 2)
        "Yes"
    ghci> primeSum (1, 6)
        "No"
-}
sumPrimes'' :: Int -> Int -> [Int] -> Bool
sumPrimes'' _ 0 _ = True 
-- sumPrimes'' 0 _ _ = True
sumPrimes'' n _ [] = n == 0
sumPrimes'' n k (p:ps) = p <= n && (sumPrimes'' (n - p) (k - 1) (p:ps) || sumPrimes'' n k ps)

primeSum :: (Int, Int) -> String
primeSum (n, k) | sumPrimes n k = "Yes"
                | otherwise = "No"
