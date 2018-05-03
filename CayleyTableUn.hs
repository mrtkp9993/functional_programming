{-
For each n>1, we define U(n) to be the set of all positive integers
less than n and relatively prime to n. Then U(n) is a group under
multiplication modulo n.
Source: Gallian, Contemporary Abstract Algebra.
-}
module CayleyTableUn where

import Data.List (intercalate)
import Euler_Totient (coprimes)

data CayleyTable = CayleyTable [[Int]]

instance Show CayleyTable where
  show (CayleyTable x) = intercalate "\n" $ map show x

cayleyTableUn :: Int -> CayleyTable
cayleyTableUn n = CayleyTable [[i * j `mod` n | i <- un] | j <- un]
  where un = coprimes n

main = do
  putStrLn "Cayley Table for U(10)"
  print $ cayleyTableUn 10
