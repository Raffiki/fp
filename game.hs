module Game where

import Control.Monad.State

type GameState = (Bool, String)

playGame :: String -> State GameState GameState
playGame x = do
  (done, score) <- get
  _ <- put (done, score ++ x)
  get

startState :: GameState
startState = (False, "")

main = loop startState
  where
    loop s = do
      input <- getLine
      let (_, (d, i)) = runState (playGame input) s
      _ <- print i
      loop (d, i)
