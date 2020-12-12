{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( prompt,
    initialState,
    parse,
    step,
    randomPlayer,
    Player,
    Board,
    hasWinner,
  )
where

import Control.Lens
import Control.Lens.TH
import Data.List
import Data.Maybe
import System.Random
import Text.Regex.Posix

type Message = String

data Command = Command {_rowIdx :: Int, _colIdx :: Int, _rotate :: Maybe (QuadrantId, String)} deriving (Show)

data Player = BlackP | WhiteP deriving (Show, Enum, Bounded)

data Board = Board {_roundNumber :: Int, _player :: Player, _q1 :: Quadrant, _q2 :: Quadrant, _q3 :: Quadrant, _q4 :: Quadrant}

data Cell = Black | White | Clean deriving (Eq)

instance Random Player where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

randomPlayer :: IO Player
randomPlayer = do
  g <- newStdGen
  return . fst $ random g

instance Show Cell where
  show Black = "b"
  show White = "w"
  show Clean = "."

data QuadrantId = One | Two | Three | Four deriving (Eq, Show)

data Row = Row {_cell1 :: Cell, _cell2 :: Cell, _cell3 :: Cell}

instance Show Row where
  show (Row c1 c2 c3) = " " ++ show c1 ++ "  " ++ show c2 ++ "  " ++ show c3

data Quadrant = Quadrant
  { _quadrantId :: QuadrantId,
    _row1 :: Row,
    _row2 :: Row,
    _row3 :: Row
  }

makeLenses ''Board
makeLenses ''Quadrant
makeLenses ''Row

nextPlayer :: Player -> Player
nextPlayer BlackP = WhiteP
nextPlayer WhiteP = BlackP

nextRound :: Board -> Board
nextRound = over roundNumber (+ 1) . over player nextPlayer

setQuadrant :: Board -> Quadrant -> Board
setQuadrant s q = set l q s
  where
    l
      | _quadrantId q == One = q1
      | _quadrantId q == Two = q2
      | _quadrantId q == Three = q3
      | _quadrantId q == Four = q4

updateQuadrant :: Quadrant -> Int -> Int -> Cell -> Either Message Quadrant
updateQuadrant q 1 1 c = updateCell (row1 . cell1) q c
updateQuadrant q 1 2 c = updateCell (row1 . cell2) q c
updateQuadrant q 1 3 c = updateCell (row1 . cell3) q c
updateQuadrant q 2 1 c = updateCell (row2 . cell1) q c
updateQuadrant q 2 2 c = updateCell (row2 . cell2) q c
updateQuadrant q 2 3 c = updateCell (row2 . cell3) q c
updateQuadrant q 3 1 c = updateCell (row3 . cell1) q c
updateQuadrant q 3 2 c = updateCell (row3 . cell2) q c
updateQuadrant q 3 3 c = updateCell (row3 . cell3) q c

getRow :: Int -> Board -> [Cell]
getRow nr (Board _ _ q1 q2 q3 q4)
  | nr <= 3 = concat $ _getRow nr <$> [q1, q2]
  | otherwise = concat $ _getRow (nr - 3) <$> [q3, q4]

getColumn :: Int -> Board -> [Cell]
getColumn nr (Board _ _ q1 q2 q3 q4)
  | nr <= 3 = concat $ _getRow nr . transposeQ <$> [q1, q3]
  | otherwise = concat $ _getRow (nr - 3) . transposeQ <$> [q2, q4]

getDiagonalq1q4 :: Board -> [Cell]
getDiagonalq1q4 (Board _ _ q1 _ _ q4) =
  [view (row1 . cell1) q1, view (row2 . cell2) q1, view (row3 . cell3) q1, view (row1 . cell1) q4, view (row2 . cell2) q4, view (row3 . cell3) q4]

getDiagonalq2q2 :: Board -> [Cell]
getDiagonalq2q2 (Board _ _ _ q2 q3 _) =
  [view (row1 . cell3) q2, view (row2 . cell2) q2, view (row3 . cell1) q2, view (row1 . cell3) q3, view (row2 . cell2) q3, view (row3 . cell1) q3]

_getRow :: Int -> Quadrant -> [Cell]
_getRow 1 (Quadrant _ (Row c1 c2 c3) _ _) = [c1, c2, c3]
_getRow 2 (Quadrant _ _ (Row c1 c2 c3) _) = [c1, c2, c3]
_getRow 3 (Quadrant _ _ _ (Row c1 c2 c3)) = [c1, c2, c3]

updateCell :: Lens' Quadrant Cell -> Quadrant -> Cell -> Either Message Quadrant
updateCell l q c = case view l q of
  Black -> Left "Cannot update cell already occupied by Black"
  White -> Left "Cannot update cell already occupied by Left"
  Clean -> Right $ set l c q

reverseQ :: Quadrant -> Quadrant
reverseQ (Quadrant id row1 row2 row3) = Quadrant id row3 row2 row1

transposeQ :: Quadrant -> Quadrant
transposeQ (Quadrant id (Row c1 c2 c3) (Row c4 c5 c6) (Row c7 c8 c9)) = Quadrant id (Row c1 c4 c7) (Row c2 c5 c8) (Row c3 c6 c9)

rotateLeft :: Quadrant -> Quadrant
rotateLeft = reverseQ . transposeQ

rotateRight :: Quadrant -> Quadrant
rotateRight = transposeQ . reverseQ

mkInitialQuadrant number =
  Quadrant
    { _quadrantId = number,
      _row1 = Row Clean Clean Clean,
      _row2 = Row Clean Clean Clean,
      _row3 = Row Clean Clean Clean
    }

initialState :: Player -> Board
initialState p = Board 0 p (mkInitialQuadrant One) (mkInitialQuadrant Two) (mkInitialQuadrant Three) (mkInitialQuadrant Four)

hasWinner :: Board -> Maybe Player
hasWinner s =
  if any allBlackOrWhite (getDiagonalq1q4 s : getDiagonalq2q2 s : map (flip getRow s) [1 .. 6] ++ map (flip getColumn s) [1 .. 6])
    then Just $ view player s
    else Nothing

verifyDone :: Command -> Board -> Board -> (Message, Maybe Board)
verifyDone (Command _ _ Nothing) oldState newState =
  case hasWinner newState of
    Just p -> (show p ++ " won", Nothing)
    Nothing -> ("You can only omit quadrant rotation when you finish the game", Just oldState)
verifyDone _ _ newState =
  case hasWinner newState of
    Just p -> (show p ++ " won", Nothing)
    Nothing -> ("", Just newState)

getQuadrantId :: Command -> QuadrantId
getQuadrantId (Command r c _)
  | (r < 3) && c < 3 = One
  | r < 3 = Two
  | c < 3 = Three
  | otherwise = Four

getQuadrant :: Board -> QuadrantId -> Quadrant
getQuadrant s qId
  | qId == One = _q1 s
  | qId == Two = _q2 s
  | qId == Three = _q3 s
  | qId == Four = _q4 s

applyRotation :: Command -> Board -> Board
applyRotation (Command _ _ Nothing) s = s
applyRotation (Command _ _ (Just (quadrantId, "l"))) s = setQuadrant s $ rotateLeft $ getQuadrant s quadrantId
applyRotation (Command _ _ (Just (quadrantId, "r"))) s = setQuadrant s $ rotateRight $ getQuadrant s quadrantId

applyCommand :: Board -> Command -> Either Message Board
applyCommand s c = (nextRound . applyRotation c) . setQuadrant s <$> updateQuadrant quadrant row col cell
  where
    quadrantId = getQuadrantId c
    quadrant = getQuadrant s quadrantId
    row = (_rowIdx c `mod` 3) + 1
    col = (_colIdx c `mod` 3) + 1
    cell = currentlyPlaying s

step :: Board -> Command -> (Message, Maybe Board)
step s c = case applyCommand s c of
  Left message -> (message, Just s)
  Right newState -> verifyDone c s newState

currentlyPlaying :: Board -> Cell
currentlyPlaying s = case mod (_roundNumber s) 2 of
  0 -> Black
  1 -> White

prompt :: Board -> Message
prompt (Board _ _ q1 q2 q3 q4) =
  "    1  2  3  4  5  6" ++ "\n a "
    ++ show (_row1 q1)
    ++ "|"
    ++ show (_row1 q2)
    ++ "\n           |\n b "
    ++ show (_row2 q1)
    ++ "|"
    ++ show (_row2 q2)
    ++ "\n           |\n c "
    ++ show (_row3 q1)
    ++ "|"
    ++ show (_row3 q2)
    ++ "\n ------------------- \n d "
    ++ show (_row1 q3)
    ++ "|"
    ++ show (_row1 q4)
    ++ "\n           |\n e "
    ++ show (_row2 q3)
    ++ "|"
    ++ show (_row2 q4)
    ++ "\n           |\n f "
    ++ show (_row3 q3)
    ++ "|"
    ++ show (_row3 q4)
    ++ "\n\n"

parse :: String -> Maybe Command
parse args = p (args =~ "^([a-f])([1-6])((I|II|III|IV)(r|l))?$" :: (String, String, String, [String]))
  where
    p (_, _, _, []) = Nothing
    p (_, _, _, [row, col, "", "", ""]) = Just $ Command (rowIndex row) (colIndex col) Nothing
    p (_, _, _, [row, col, _, "I", direction]) = Just $ Command (rowIndex row) (colIndex col) $ Just (One, direction)
    p (_, _, _, [row, col, _, "II", direction]) = Just $ Command (rowIndex row) (colIndex col) $ Just (Two, direction)
    p (_, _, _, [row, col, _, "III", direction]) = Just $ Command (rowIndex row) (colIndex col) $ Just (Three, direction)
    p (_, _, _, [row, col, _, "IV", direction]) = Just $ Command (rowIndex row) (colIndex col) $ Just (Four, direction)

    rowIndex row = fromMaybe (error "row index out of bounds") $ row `elemIndex` ["a", "b", "c", "d", "e", "f"]
    colIndex col = fromMaybe (error "column index out of bounds") $ col `elemIndex` ["1", "2", "3", "4", "5", "6"]

allSameOf :: (Eq a) => [a] -> [a] -> Bool
allSameOf _ [] = True
allSameOf legalValues (x : xs) = elem x legalValues && all (== x) xs

allBlackOrWhite :: [Cell] -> Bool
allBlackOrWhite = allSameOf [Black, White]
