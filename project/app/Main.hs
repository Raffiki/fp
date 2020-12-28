module Main where

import Lib
import Ui

main :: IO ()
main =
  do
    startingPlayer <- randomPlayer
    runUI startingPlayer (nextPlayer startingPlayer)
