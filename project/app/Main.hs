module Main where

import Lib

main :: IO ()
main =
  do
    startingPlayer <- randomPlayer
    loop $ Just $ initialState startingPlayer
  where
    loop Nothing = return ()
    loop (Just s) = do
      putStrLn $ prompt s
      input <- getLine
      let (message, newState) = case parse input of
            Nothing -> ("Cannot parse input. try again", Just s)
            Just c -> step s c
      putStrLn message
      loop newState