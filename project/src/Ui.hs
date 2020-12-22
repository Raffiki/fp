module Ui (runUI) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib

type Coordinates = (Float, Float)

data PartialCommand = PartialCommand (Maybe (Int, Int)) (Maybe QuadrantId) (Maybe Direction)

data World = World
  { _board :: Board,
    _message :: Maybe Message,
    _command :: Either PartialCommand Command
  }

type Size = Float

initialCommand :: PartialCommand
initialCommand = PartialCommand Nothing Nothing Nothing

emptyWorld :: Player -> World
emptyWorld p = World (initialBoard p) Nothing $ Left initialCommand

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
    toCoordinates (player, (row, col)) = (player, (fromInteger (toInteger col) - 3.5, (fromInteger (toInteger row) -3.5) * (-1)))

drawBoard :: Size -> World -> Picture
drawBoard k (World b m _) = Pictures $ grid : bs ++ ws
  where
    bs = drawCircle k black . snd <$> (\(p, c) -> p == Black) `filter` getPlayerCoordinates b
    ws = drawCircle k white . snd <$> (\(p, c) -> p == White) `filter` getPlayerCoordinates b

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

handleKeys :: Size -> Event -> World -> World
handleKeys k (EventKey (MouseButton LeftButton) Down _ (x', y')) w@(World b _ (Left (PartialCommand Nothing _ _))) =
  fromMaybe w $ do
    c <- getColumnIndexFromClick k x'
    r <- getRowIndexFromClick k y'
    return $ trace (show (r, c)) (World b (Just "click on a quadrant to rotate") (Left (PartialCommand (Just (r, c)) Nothing Nothing)))
handleKeys k (EventKey (MouseButton LeftButton) Down _ (x', y')) w@(World b _ (Left (PartialCommand (Just pos) Nothing _))) =
  fromMaybe w $ do
    c <- getColumnIndexFromClick k x'
    r <- getRowIndexFromClick k y'
    return $ trace (show (getQuadrantId (r, c))) (World b (Just "click 'l' or 'r' key to rotate left/right") (Left (PartialCommand (Just pos) (Just (getQuadrantId (r, c))) Nothing)))
handleKeys k (EventKey (Char 'l') Up _ _) w@(World b _ (Left (PartialCommand (Just (r, c)) (Just qId) Nothing))) =
  case applyCommand b (Command r c (Just (qId, "l"))) of
    Left message -> w
    Right b -> World b (Just "click on a cell") $ Left initialCommand
handleKeys k (EventKey (Char 'r') Up _ _) w = trace (show "right") w
handleKeys k _ w = w

runUI :: Player -> IO ()
runUI p =
  let window = InWindow "Pentago" (800, 600) (10, 10)
      size = 100.0
   in play window yellow 1 (emptyWorld p) (drawBoard size) (handleKeys size) (flip const)