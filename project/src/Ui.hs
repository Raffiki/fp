module Ui (runUI) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib

type Coordinates = (Int, Int)

data PartialCommand = PartialCommand (Maybe (Int, Int)) (Maybe QuadrantId) (Maybe Direction)

data World = World
  { _board :: Board,
    _command :: Either PartialCommand Command
  }

type Size = Float

initialCommand :: PartialCommand
initialCommand = PartialCommand Nothing Nothing Nothing

emptyWorld :: Player -> World
emptyWorld p = World (initialBoard p) $ Left initialCommand

resize :: Size -> Path -> Path
resize k = fmap (\(x, y) -> (x * k, y * k))

drawCircle :: Size -> Color -> Coordinates -> Picture
drawCircle k c (x, y) =
  let x' = k * fromIntegral x
      y' = k * fromIntegral y
   in color c $ translate x' y' $ thickCircle (0.1 * k) (0.3 * k)

getPlayerCoordinates :: Board -> [(Player, Coordinates)]
getPlayerCoordinates b = toCoordinates <$> getPositions b
  where
    toCoordinates :: Position -> (Player, Coordinates)
    toCoordinates (player, row, col) = (player, ((row - 2) * (-1), col -2))

drawBoard :: Size -> World -> Picture
drawBoard k (World b _) = Pictures $ grid : bs ++ ws
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
handleKeys k (EventKey (MouseButton LeftButton) Down _ (x', y')) b =
  fromMaybe b $ do
    c <- getColumnIndexFromClick k x'
    r <- getRowIndexFromClick k y'
    return $ trace (show (r, c)) b
handleKeys k _ w = w

runUI :: Player -> IO ()
runUI p =
  let window = InWindow "Pentago" (800, 600) (10, 10)
      size = 100.0
   in play window yellow 1 (emptyWorld p) (drawBoard size) (handleKeys size) (flip const)