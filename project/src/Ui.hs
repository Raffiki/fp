{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ui (runUI, emptyWorld) where

import Control.Applicative
import Control.Concurrent
import Control.Lens (Lens', makeLenses, over, set, view)
import Control.Lens.TH ()
import Control.Monad.State
import Data.Either
import Data.Maybe
import Debug.Trace
import Errors
import Exploration
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Lib
import Parser

type Coordinates = (Float, Float)

data PartialCommand = PartialCommand
  { _position :: (Maybe (Int, Int)),
    _quadrantId :: (Maybe QuadrantId),
    _rotation :: (Maybe Rotation)
  }

data World = World
  { _gameState :: GameState,
    _aiPlayer :: Player,
    _isBusy :: Maybe (MVar Bool),
    _command :: PartialCommand
  }

type GameOver = Bool

data History s = History {current :: s, undos :: [s]}
  deriving (Eq, Show, Read)

blankHistory s = History {current = s, undos = []}

data GameState = GameState
  { _gameOver :: GameOver,
    _history :: History (Board, Maybe Move),
    _message :: Maybe Message
  }

type Size = Float

makeLenses ''GameState
makeLenses ''PartialCommand
makeLenses ''World

initialCommand :: PartialCommand
initialCommand = PartialCommand Nothing Nothing Nothing

emptyWorld :: Player -> Player -> World
emptyWorld p aiPlayer = World (GameState False (blankHistory (initialBoard p, Nothing)) Nothing) aiPlayer Nothing initialCommand

resize :: Size -> Path -> Path
resize k = fmap (\(x, y) -> (x * k, y * k))

drawCircle :: Size -> Color -> Coordinates -> Picture
drawCircle k c (x, y) =
  let x' = k * x
      y' = k * y
   in color c $ translate x' y' $ thickCircle (0.1 * k) (0.3 * k)

getPlayerCoordinates :: Board -> [(Player, Coordinates)]
getPlayerCoordinates b = toCoordinates <$> getPositions b
  where
    toCoordinates :: (Player, Position) -> (Player, Coordinates)
    toCoordinates (player, Position (row, col)) = (player, (fromInteger (toInteger col) - 3.5, (fromInteger (toInteger row) -3.5) * (-1)))

drawText :: Size -> Message -> Picture
drawText k m = Color black $ translate (2 * k) (- k) $ scale 0.1 0.1 $ text m

topText :: Size -> Player -> Picture
topText k Black =
  Pictures
    [ color black (translate (2 * k) (2.5 * k) $ scale 0.1 0.1 $ text "AI player"),
      color white $ translate (2 * k) (2 * k) $ scale 0.1 0.1 $ text "Human"
    ]
topText k White =
  Pictures
    [ color white (translate (2 * k) (2.5 * k) $ scale 0.1 0.1 $ text "AI player"),
      color black $ translate (2 * k) (2 * k) $ scale 0.1 0.1 $ text "Human"
    ]

drawBoard :: Size -> World -> Picture
drawBoard k (World (GameState True _ (Just m)) aiPlayer _ _) = Pictures [toptext, drawText k m]
  where
    toptext = topText k aiPlayer
drawBoard k (World (GameState False (History (b, _) _) m) aiPlayer _ _) = Pictures $ load : grid : toptext : message ++ bs ++ ws
  where
    toptext = topText k aiPlayer
    message = maybeToList $ fmap (drawText k) m
    bs = drawCircle k black . snd <$> (\(p, c) -> p == Black) `filter` getPlayerCoordinates b
    ws = drawCircle k white . snd <$> (\(p, c) -> p == White) `filter` getPlayerCoordinates b

    load :: Picture
    load =
      color black $
        Pictures $
          color black (translate (2.2 * k) (-2.7 * k) $ scale 0.1 0.1 $ text "LOAD") :
          fmap
            (line . resize k)
            [ [(2.1, -2.9), (2.9, -2.9)],
              [(2.1, -2.4), (2.9, -2.4)],
              [(2.1, -2.9), (2.1, -2.4)],
              [(2.9, -2.4), (2.9, -2.9)]
            ]

    grid :: Picture
    grid =
      color black $
        Pictures $
          fmap
            (line . resize k)
            [ [(-3, -3), (-3, 3)],
              [(-2, -3), (-2, 3)],
              [(-1, -3), (-1, 3)],
              [(0, -3), (0, 3)],
              [(1, -3), (1, 3)],
              [(2, -3), (2, 3)],
              [(-4, 2), (2, 2)],
              [(-4, 1), (2, 1)],
              [(-4, 0), (2, 0)],
              [(-4, -1), (2, -1)],
              [(-4, -2), (2, -2)],
              [(-4, -3), (2, -3)]
            ]

getRowIndexFromClick :: Size -> Float -> Maybe Int
getRowIndexFromClick k f' =
  let f = f' / k
   in 5 <$ guard (-3 < f && f < -2)
        <|> 4 <$ guard (-2 < f && f < -1)
        <|> 3 <$ guard (-1 < f && f < 0)
        <|> 2 <$ guard (0 < f && f < 1)
        <|> 1 <$ guard (1 < f && f < 2)
        <|> 0 <$ guard (2 < f && f < 3)

getColumnIndexFromClick :: Size -> Float -> Maybe Int
getColumnIndexFromClick k f' =
  let f = f' / k
   in 0 <$ guard (-4 < f && f < -3)
        <|> 1 <$ guard (-3 < f && f < -2)
        <|> 2 <$ guard (-2 < f && f < -1)
        <|> 3 <$ guard (-1 < f && f < 0)
        <|> 4 <$ guard (0 < f && f < 1)
        <|> 5 <$ guard (1 < f && f < 2)

handleKeys :: Size -> Event -> World -> IO World
handleKeys k (EventKey (MouseButton LeftButton) Down _ (x', y')) w = handleLeftClick k (x', y') w
handleKeys _ (EventKey (Char 'l') Up _ _) w@(World gs _ _ (PartialCommand (Just (r, c)) (Just qId) Nothing)) =
  return $ set command initialCommand $ set gameState newState w
  where
    move = Left (NormalMove (Position (r, c)) qId L)
    (_, newState) = runState (playGame move) gs
handleKeys _ (EventKey (Char 'r') Up _ _) w@(World gs _ _ (PartialCommand (Just (r, c)) (Just qId) Nothing)) =
  return $ set command initialCommand $ set gameState newState w
  where
    move = Left (NormalMove (Position (r, c)) qId R)
    (_, newState) = runState (playGame move) gs
handleKeys _ _ w = return w

handleBoardLeftClick :: Position -> World -> IO World
handleBoardLeftClick pos w@(World _ _ _ (PartialCommand (Just _) Nothing _)) =
  return $
    set (command . quadrantId) (Just (getQuadrantId pos)) $
      set (gameState . message) (Just "click 'l' or 'r' key to \nrotate left/right") w
handleBoardLeftClick pos w@(World gs _ _ (PartialCommand Nothing _ _)) =
  let (_, newState) = runState (playGame (Right pos)) gs
   in return $
        if view gameOver newState
          then set gameState newState w
          else
            set (command . position) (Just (getPos pos)) $
              set (gameState . message) (Just "click on a quadrant to rotate") w

loadSavedWorld :: World -> IO World
loadSavedWorld w = do
  result <- readFromFile
  return $ case trace (show result) result of
    Left (MissingFileError e) -> set (gameState . message) (Just e) w
    Left (AppParseError e) -> set (gameState . message) (Just e) w
    Right (humanPlayer, startPlayer, moves) -> applyMoves moves (emptyWorld humanPlayer humanPlayer)
      where
        applyMoves :: MoveSequence -> World -> World
        applyMoves (a :> Empty) world = world
        applyMoves (a :> End p) world = set gameState (execState (playGame (Right p)) (view gameState world)) world
        applyMoves (a :> as) world = applyMoves as $ set gameState (execState (playGame (Left a)) (view gameState world)) world

historyToMoveSequence :: History (Board, Maybe Move) -> MoveSequence
historyToMoveSequence (History (b, Just (Left move)) moves) = foldr (:>) (move :> Empty) (lefts (catMaybes (snd <$> moves)))
historyToMoveSequence (History (b, Just (Right endMove)) moves) = foldr (:>) (previousMove :> End endMove) previousMoves
  where
    (previousMove : previousMoves) = lefts (catMaybes (snd <$> moves))

saveWorld :: World -> IO ()
saveWorld (World (GameState _ history _) aiPlayer _ _) = writeToFile (nextPlayer aiPlayer, nextPlayer aiPlayer, historyToMoveSequence history)

handleMenuLeftClick :: (Float, Float) -> World -> IO World
handleMenuLeftClick (x, y) w = handleClick
  where
    handleClick
      | x > 2.1 && x < 2.9 && y > -2.9 && y < -2.4 = loadSavedWorld w
      | x > 2.1 && x < 2.9 && y > -2.4 && y < -1.8 = saveWorld w >> return w
      | otherwise = return w

handleLeftClick :: Size -> (Float, Float) -> World -> IO World
handleLeftClick k (x, y) w = fromMaybe (handleMenuLeftClick (x / k, y / k) w) $ do
  c <- getColumnIndexFromClick k x
  r <- getRowIndexFromClick k y
  return $ handleBoardLeftClick (Position (r, c)) w

step :: Board -> Move -> (GameOver, Message, Board)
step b m = case applyMove b m of
  Left message -> (False, message, b)
  Right newBoard -> verifyDone m b newBoard

playGame :: Move -> State GameState GameState
playGame move = do
  GameState _ (History (b, m) undos) _ <- get
  let (gameOver, message, newBoard) = step b move
  _ <- put (GameState gameOver (History (newBoard, Just move) ((b, m) : undos)) (Just message))
  get

verifyDone :: Move -> Board -> Board -> (GameOver, Message, Board)
verifyDone (Right _) oldBoard newBoard =
  if _player oldBoard `elem` getWinners newBoard
    then (True, "Player " ++ show (_player oldBoard) ++ " won", newBoard)
    else (False, "You can only omit quadrant rotation when *you* win the game", oldBoard)
verifyDone _ _ newBoard = case getWinners newBoard of
  [_, _] -> (True, "You both won", newBoard)
  [winner] -> (True, show winner ++ " won", newBoard)
  _ -> (False, "", newBoard)

stepWorld :: Float -> World -> IO World
stepWorld _ w@(World (GameState True _ _) _ _ _) = return w --gameOver
stepWorld _ w@(World _ _ Nothing _) = do
  busy <- newMVar False
  return $ set isBusy (Just busy) w
stepWorld _ w@(World gs@(GameState _ (History (b@(Board player _ _ _ _), _) _) _) aiPlayer (Just busy) _) =
  if player /= aiPlayer
    then return w
    else do
      -- AI's turn
      isBusy <- takeMVar busy
      putMVar busy True
      if isBusy
        then -- AI is thinking
          return w
        else case nextAICommand aiPlayer b of
          Nothing -> trace (show "AI cannot find move...") return w
          Just m ->
            takeMVar busy >> putMVar busy False
              >> return newWorld
            where
              (_, newState) = runState (playGame m) gs
              newWorld = set command initialCommand $ set gameState newState w

runUI :: World -> IO ()
runUI world =
  let window = InWindow "Pentago" (800, 600) (10, 10)
      size = 100.0
   in playIO window yellow 1 world (pure <$> drawBoard size) (handleKeys size) stepWorld
