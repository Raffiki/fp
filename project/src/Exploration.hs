module Exploration where

import Control.Parallel.Strategies
import Data.Either
import Data.Either.Combinators
import qualified Data.List.NonEmpty as NonEmpty
import Lib

data Forest a = a :+ [Forest a]

evalPar :: Board -> [Command] -> [(Board, Bool)]
evalPar b cs = rights (map result cs `using` parList rseq)
  where
    result c = (\b -> (b, hasWinner b)) `mapRight` applyCommand b c

buildForest :: Int -> (Board, Bool) -> Forest Board
buildForest 0 (b, _) = b :+ []
buildForest n (b, True) = b :+ [buildForest (n - 1) (b, True)]
buildForest n (b, False) = b :+ (buildForest (n - 1) <$> evalPar b allPossibleCommands)

minimax :: Player -> Forest Board -> Int
minimax p (b :+ []) = fitness b
minimax Black (_ :+ rs) = maximum (map (minimax White) rs `using` parList rseq)
minimax White (_ :+ rs) = minimum (map (minimax Black) rs `using` parList rseq)

fitness :: Board -> Int
fitness b =
  sum $
    fitnessRow
      . NonEmpty.fromList
      <$> ( getDiagonalq1q4 b :
            getDiagonalq2q2 b :
            fmap (`getRow` b) [1 .. 6]
              ++ fmap (`getColumn` b) [1 .. 6]
          )

fitnessRow :: NonEmpty.NonEmpty Cell -> Int
fitnessRow cs = fst . foldl fitnessAccumulator (0, (NonEmpty.head cs, 1)) $ NonEmpty.tail cs
  where
    fitnessAccumulator (total, (B, cnt)) B = (total, (B, cnt + 1))
    fitnessAccumulator (total, (W, cnt)) W = (total, (W, cnt + 1))
    fitnessAccumulator (total, (W, cnt)) B = (total - addToCount cnt, (B, 1))
    fitnessAccumulator (total, (B, cnt)) W = (total + addToCount cnt, (W, 1))
    addToCount 1 = 1
    addToCount 2 = 3
    addToCount 3 = 6
    addToCount 4 = 10
    addToCount 5 = maxBound :: Int
    addToCount 6 = maxBound :: Int
