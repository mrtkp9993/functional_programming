module Hamming where

hamming str1 str2 = sum (map (\(x, y) -> check_eq x y) (zip str1 str2))
  where check_eq x y
          | x == y    = 0
          | otherwise = 1
