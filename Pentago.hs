{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (Lens', makeLenses, over, set, view)
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.Regex.Posix ((=~))

type Message = String

data Command = Command {_rowIdx :: Int, _colIdx :: Int, _rotate :: Maybe (QuadrantId, String)} deriving (Show)

data Board = Board {_player :: Player, _q1 :: Quadrant, _q2 :: Quadrant, _q3 :: Quadrant, _q4 :: Quadrant}

data Cell = B | W | E deriving (Eq)

instance Show Cell where
  show B = "b"
  show W = "w"
  show E = "."

data Player = Black | White deriving (Eq, Ord, Show)

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

makeLenses ''Quadrant
makeLenses ''Row
makeLenses ''Board

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black

nextRound :: Board -> Board
nextRound = over player nextPlayer

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
getRow nr (Board _ q1 q2 q3 q4)
  | nr <= 3 = concat $ _getRow nr <$> [q1, q2]
  | otherwise = concat $ _getRow (nr - 3) <$> [q3, q4]

getColumn :: Int -> Board -> [Cell]
getColumn nr (Board _ q1 q2 q3 q4)
  | nr <= 3 = concat $ _getRow nr . transposeQ <$> [q1, q3]
  | otherwise = concat $ _getRow (nr - 3) . transposeQ <$> [q2, q4]

getDiagonalq1q4 :: Board -> [Cell]
getDiagonalq1q4 (Board _ q1 _ _ q4) =
  [view (row1 . cell1) q1, view (row2 . cell2) q1, view (row3 . cell3) q1, view (row1 . cell1) q4, view (row2 . cell2) q4, view (row3 . cell3) q4]

getDiagonalq2q2 :: Board -> [Cell]
getDiagonalq2q2 (Board _ _ q2 q3 _) =
  [view (row1 . cell3) q2, view (row2 . cell2) q2, view (row3 . cell1) q2, view (row1 . cell3) q3, view (row2 . cell2) q3, view (row3 . cell1) q3]

_getRow :: Int -> Quadrant -> [Cell]
_getRow 1 (Quadrant _ (Row c1 c2 c3) _ _) = [c1, c2, c3]
_getRow 2 (Quadrant _ _ (Row c1 c2 c3) _) = [c1, c2, c3]
_getRow 3 (Quadrant _ _ _ (Row c1 c2 c3)) = [c1, c2, c3]

updateCell :: Lens' Quadrant Cell -> Quadrant -> Cell -> Either Message Quadrant
updateCell l q c = case view l q of
  B -> Left "Cannot update cell already occupied by Black"
  W -> Left "Cannot update cell already occupied by Left"
  E -> Right $ set l c q

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
      _row1 = Row E E E,
      _row2 = Row E E E,
      _row3 = Row E E E
    }

initialBoard :: Board
initialBoard = Board Black (mkInitialQuadrant One) (mkInitialQuadrant Two) (mkInitialQuadrant Three) (mkInitialQuadrant Four)

maybeToSet :: Maybe a -> Set.Set a
maybeToSet (Just a) = Set.singleton a
maybeToSet Nothing = Set.empty

-- There is a possibility both players win by a single move
getWinners :: Board -> Set.Set Player
getWinners b = mconcat $ maybeToSet <$> fiveInARow <$> (getDiagonalq1q4 b : getDiagonalq2q2 b : fmap (flip getRow b) [1 .. 6] ++ fmap (flip getColumn b) [1 .. 6])

verifyDone :: Command -> Board -> Board -> (Message, Maybe Board)
verifyDone (Command _ _ Nothing) oldBoard newBoard =
  if _player oldBoard `elem` getWinners newBoard
    then ("You won", Nothing)
    else ("You can only omit quadrant rotation when *you* win the game", Just oldBoard)
verifyDone _ _ newBoard = case Set.elems (getWinners newBoard) of
  [_, _] -> ("You both won", Nothing)
  [winner] -> (show winner ++ " won", Nothing)
  _ -> ("", Just newBoard)

getQuadrantId :: Command -> QuadrantId
getQuadrantId (Command r c _)
  | (r < 3) && c < 3 = One
  | r < 3 = Two
  | c < 3 = Three
  | otherwise = Four

getQuadrant :: Board -> QuadrantId -> Quadrant
getQuadrant b qId
  | qId == One = _q1 b
  | qId == Two = _q2 b
  | qId == Three = _q3 b
  | qId == Four = _q4 b

applyRotation :: Command -> Board -> Board
applyRotation (Command _ _ Nothing) s = s
applyRotation (Command _ _ (Just (quadrantId, "l"))) s = setQuadrant s $ rotateLeft $ getQuadrant s quadrantId
applyRotation (Command _ _ (Just (quadrantId, "r"))) s = setQuadrant s $ rotateRight $ getQuadrant s quadrantId

getQuadrantLens :: QuadrantId -> Lens' Board Quadrant
getQuadrantLens quadrantId
  | quadrantId == One = q1
  | quadrantId == Two = q2
  | quadrantId == Three = q3
  | quadrantId == Four = q4

applyCommand :: Board -> Command -> Either Message Board
applyCommand b c = (nextRound . applyRotation c) . setQuadrant b <$> updateQuadrant quadrant row col cell
  where
    quadrantId = getQuadrantId c
    quadrant = getQuadrant b quadrantId
    row = (_rowIdx c `mod` 3) + 1
    col = (_colIdx c `mod` 3) + 1
    cell = case _player b of
      Black -> B
      White -> W

step :: Board -> Command -> (Message, Maybe Board)
step b c = case applyCommand b c of
  Left message -> (message, Just b)
  Right newBoard -> verifyDone c b newBoard

prompt :: Board -> Message
prompt (Board _ q1 q2 q3 q4) =
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

main = loop $ Just initialBoard
  where
    loop Nothing = return ()
    loop (Just s) = do
      putStrLn $ prompt s
      input <- getLine
      let (message, newBoard) = case parse input of
            Nothing -> ("Cannot parse input. try again", Just s)
            Just c -> step s c
      putStrLn message
      loop newBoard

parse :: String -> Maybe Command
parse args = p (args =~ "^([a-f])([1-6])(,(I|II|III|IV)(r|l))?$" :: (String, String, String, [String]))
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

fiveInARow :: [Cell] -> Maybe Player
fiveInARow [_, B, B, B, B, B] = Just Black
fiveInARow [B, B, B, B, B, _] = Just Black
fiveInARow [_, W, W, W, W, W] = Just White
fiveInARow [W, W, W, W, W, _] = Just White
fiveInARow _ = Nothing
