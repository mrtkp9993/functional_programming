module ClosestPair where

import Data.List
import Data.Ord

data Point = Point Float Float

instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- Don't need Eq in this solution
instance Eq Point where
  Point x1 y1 == Point x2 y2 = x1 == x2 && y1 == y2

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

closestPair :: [Point] -> (Point, Point, Float)
closestPair pointList = do
  let distances = [(x, y, distance x y) | (x:ys) <- tails pointList, y <- ys]
  minimumBy (comparing (\(_,_,dist) -> dist)) distances

main :: (Point, Point, Float)
main = do
  let points = [Point 2 3, Point 12 30, Point 40 50, Point 5 1, Point 12 10, Point 3 4]
  closestPair points
