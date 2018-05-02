module Euler_Totient where

coprimes :: Integral a => a -> [a]
coprimes n = [x | x <- [1..(n-1)], gcd n x == 1]

totient :: Integral a => a -> Int
totient n = length $ coprimes n
