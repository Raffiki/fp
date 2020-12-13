module Exploration where

import qualified Data.List.NonEmpty as NonEmpty
import Lib
  ( Board,
    Cell (B, W),
    Player (..),
    getColumn,
    getDiagonalq1q4,
    getDiagonalq2q2,
    getRow,
  )

data Forest a = a :+ [Forest a]

buildForest :: Int -> Board -> Forest Board
buildForest 0 b = b :+ []
-- buildForest n b = 




minimax :: Player -> Forest Board -> Int
minimax p (b :+ []) = fitness b
minimax Black (_ :+ rs) = maximum $ map (minimax White) rs
minimax White (_ :+ rs) = minimum $ map (minimax Black) rs

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
fitnessRow cs = fst . foldl fitmessAccumulator (0, (NonEmpty.head cs, 1)) $ NonEmpty.tail cs
  where
    fitmessAccumulator (total, (B, cnt)) B = (total, (B, cnt + 1))
    fitmessAccumulator (total, (W, cnt)) W = (total, (W, cnt + 1))
    fitmessAccumulator (total, (W, cnt)) B = (total - addToCount cnt, (B, 1))
    fitmessAccumulator (total, (B, cnt)) W = (total + addToCount cnt, (W, 1))
    addToCount 1 = 1
    addToCount 2 = 3
    addToCount 3 = 6
    addToCount 4 = 10
    addToCount 5 = maxBound :: Int
    addToCount 6 = maxBound :: Int
