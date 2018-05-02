module Quickshort where

quickshort :: Ord a => [a] -> [a]
quickshort [] = []
quickshort (x:xs) = quickshort less ++ [x] ++ quickshort more
  where less = filter (<x) xs
        more = filter (>=x) xs
