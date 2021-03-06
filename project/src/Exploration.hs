module Exploration (nextAICommand) where

import Control.Parallel.Strategies
import Data.Either
import Data.Either.Combinators
import qualified Data.List.NonEmpty as NonEmpty
import Debug.Trace
import Lib

data Forest a = a :+ [Forest a]

buildForest :: Int -> ((Board, Move), Bool) -> Forest (Board, Move)
buildForest 0 (b, _) = b :+ []
buildForest n (b, True) = b :+ [buildForest (n - 1) (b, True)]
buildForest n (b, False) = b :+ (buildForest (n - 1) <$> evalPar b allPossibleCommands)
  where
    evalPar :: (Board, Move) -> [Move] -> [((Board, Move), Bool)]
    evalPar (b, c) cs = rights (map result cs `using` parList rseq)
      where
        result c = (\b -> ((b, c), hasWinner b)) `mapRight` applyMove b c

minimax :: Player -> Forest (Board, Move) -> (Int, [Move])
minimax p ((b, c) :+ []) = (fitness b, [c])
minimax Black ((b, c) :+ rs) =
  let (n, cs) = maximum (map (minimax White) rs `using` parList rseq)
   in (n, c : cs)
minimax White ((b, c) :+ rs) =
  let (n, cs) = minimum (map (minimax Black) rs `using` parList rseq)
   in (n, c : cs)

nextAICommands :: Player -> Board -> (Int, [Move])
nextAICommands aiPlayer b = minimax aiPlayer $ buildForest 2 ((b, dummyCommand), False)
  where
    dummyCommand = Left $ NormalMove (Position (1, 1)) One L

nextAICommand :: Player -> Board -> Maybe Move
nextAICommand p b = case nextAICommands p b of
  (_, _ : second : _) -> trace (show second) Just second
  _ -> Nothing

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
    fitnessAccumulator :: (Int, (Cell, Int)) -> Cell -> (Int, (Cell, Int))
    fitnessAccumulator (total, (B, cnt)) B = (total, (B, cnt + 1))
    fitnessAccumulator (total, (W, cnt)) W = (total, (W, cnt + 1))
    fitnessAccumulator (total, (E, cnt)) E = (total, (E, cnt + 1))
    fitnessAccumulator (total, (W, cnt)) B = (total - addToCount cnt, (B, 1))
    fitnessAccumulator (total, (E, cnt)) B = (total, (B, 1))
    fitnessAccumulator (total, (W, cnt)) E = (total - addToCount cnt, (E, 1))
    fitnessAccumulator (total, (B, cnt)) E = (total - addToCount cnt, (E, 1))
    fitnessAccumulator (total, (B, cnt)) W = (total + addToCount cnt, (W, 1))
    fitnessAccumulator (total, (E, cnt)) W = (total, (W, 1))

    addToCount :: Int -> Int
    addToCount 1 = 1
    addToCount 2 = 3
    addToCount 3 = 6
    addToCount 4 = 10
    addToCount 5 = maxBound :: Int
    addToCount 6 = maxBound :: Int
