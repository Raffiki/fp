module Game where

import Control.Monad.State

type GameState = (Bool, History String)

data History s = History {current :: s, undos :: [s]}
  deriving (Eq, Show, Read)

blankHistory s = History {current = s, undos = []}

playGame :: String -> State GameState GameState
playGame "undo" = do
  (done, his) <- get
  _ <- case undos his of
    (u : us) -> put (done, History {current = u, undos = us})
    _ -> return ()
  get
playGame x = do
  (done, his) <- get
  _ <- put (done, History {current = x, undos = current his : undos his})
  get

-- _      <- put $ (done, History { current = x, undos = current his : undos his })
-- get

startState :: GameState
startState = (False, blankHistory "")

main = loop startState
  where
    loop s = do
      input <- getLine
      let (_, (d, i)) = runState (playGame input) s
      _ <- print $ current i
      loop (d, i)
