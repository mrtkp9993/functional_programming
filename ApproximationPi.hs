import System.Environment
import System.Random
import Control.Monad

pickPointAndCheck :: IO Integer
pickPointAndCheck = do
    randX <- randomRIO (-1, 1)
    randY <- randomRIO (-1, 1)
    let dist :: Double
        dist = sqrt (randX * randX + randY * randY)
    return (if dist < 1 then 1 else 0)

approximate_pi :: Fractional a => Int -> IO a
approximate_pi n = do
    trials <- replicateM n pickPointAndCheck
    let m = foldl (+) 0 trials
    return (4 * fromIntegral m / fromIntegral n)

main :: IO ()
main = do
    args <- getArgs
    let n = args !! 0
    res <- approximate_pi (read n :: Int)
    print $ res
    