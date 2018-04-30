module Hamming where

hamming :: (Num a, Eq b) => [b] -> [b] -> a
hamming str1 str2 = sum (map (\(x, y) -> check_eq x y) (zip str1 str2))
  where check_eq x y
          | x == y    = 0
          | otherwise = 1
