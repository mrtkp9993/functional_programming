module NumberTheory where

isDivides :: Integral a => a -> a -> Bool
isDivides a b
  | b `mod` a == 0 = True
  | otherwise  = False

myGCD :: Integral a => a -> a -> a
myGCD a b
  | a == 0 && b == 0 = 0
  | a == 0 || b == 0 = a
  | otherwise        = maximum [d | d <- [1..mab], isDivides d a, isDivides d b]
    where mab = min a b

factors :: Integral a => a -> [a]
factors n = [k | k <- [1..n], mod n k == 0]

coprimes :: Integral a => a -> [a]
coprimes n = [x | x <- [1..(n-1)], myGCD n x == 1]

totient :: Integral a => a -> Int
totient n = length $ coprimes n

isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && []==[i | i <- [2..n-1], rem n i == 0]

primes :: Integral a => a -> [a]
primes n = [p | p <- [2..n], isPrime p]
