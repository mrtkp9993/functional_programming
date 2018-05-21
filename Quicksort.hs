module Quicksort where

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort less ++ [x] ++ quicksort more
  where less = filter (<x) xs
        more = filter (>=x) xs
