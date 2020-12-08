-- module ParQuickSort where

import Control.Monad.Par (NFData)
import Control.Monad.Par.Scheds.Trace
import System.Random

randomInts :: Int -> [Int]
randomInts n = take n . randoms $ mkStdGen 255

parQuickSort :: (Ord a, NFData a) => [a] -> Par [a]
parQuickSort [] = return []
parQuickSort (x : xs) = do
  p1 <- spawn (parQuickSort (filter (< x) xs))
  p2 <- spawn (parQuickSort (filter (>= x) xs))
  left <- get p1
  right <- get p2
  return $ left ++ (x : right)

main = print $ runPar . parQuickSort $ randomInts 20000
