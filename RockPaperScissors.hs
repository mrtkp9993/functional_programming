{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RockPaperScissors where

import Control.Exception
import Control.Monad
import System.Random

data Move = Rock | Paper | Scissors deriving (Show, Enum, Read)

instance Eq Move where
  Rock     == Rock     = True
  Paper    == Paper    = True
  Scissors == Scissors = True
  _        == _        = False
  x        /= y        = not (x == y)

instance (Eq Move) => Ord Move where
  (<=)  x y = x == y || elem (x, y) [(Rock, Paper),(Paper, Scissors),
                                     (Scissors, Rock)]

data MoveError = ParseError deriving (Show, Eq, Read)

input :: IO Move
input = fmap read getLine

randMove :: IO Move
randMove =  fmap ([Paper, Rock, Scissors] !!) $ randomRIO(0, 2)

score :: Ordering -> IO ()
score ord = case ord of
  EQ -> putStrLn "Tie!"
  LT -> putStrLn "Computer won!"
  GT -> putStrLn "You won!"

main = forever $ do
  putStrLn "Choose your move (Rock/Paper/Scissors): "
  player <- input
  catch (do
            let parsedInput = show player
            putStrLn parsedInput
            computer <- randMove
            putStrLn ("Computer's choice is: " ++ show (computer))
            score (compare player computer)
        )
        (\(err:: SomeException) -> do
            putStrLn "Please re-enter your choice!"
        )
