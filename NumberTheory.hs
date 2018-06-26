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

coprimes :: Integral a => a -> [a]
coprimes n = [x | x <- [1..(n-1)], gcd n x == 1]

totient :: Integral a => a -> Int
totient n = length $ coprimes n
