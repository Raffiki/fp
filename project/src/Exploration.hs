module Exploration where

import Lib

data Forest a = a :> [Forest a]

minimax :: Player -> Forest Board -> Int
minimax p (b :> [])
  | hasWinner b == Just p = 1
  | hasWinner r == Just P2 = (-1)
  | otherwise = 0
minimax BlackP (r :> rs) = maximum (map root xs) :> xs
  where
    xs = map (minimax (nextPlayer p)) rs
minimax BlackP (r :> rs) = minimum (map root xs) :> xs
  where
    xs = map (minimax (nextPlayer p)) rs