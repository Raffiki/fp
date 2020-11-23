{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Lens.TH
import Data.List
import Data.Maybe
import Text.Regex.Posix

type Message = String

data Command = Command {_rowIdx :: Int, _colIdx :: Int, _rotate :: Maybe (QuadrantId, String)} deriving (Show)

data State = State {_roundNumber :: Int, _q1 :: Quadrant, _q2 :: Quadrant, _q3 :: Quadrant, _q4 :: Quadrant}

data Cell = Black | White | Clean

data Continue a = Done a | NotDone a

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

makeLenses ''Quadrant
makeLenses ''Row
makeLenses ''State

nextState :: State -> State
nextState = over roundNumber (+ 1)

setQuadrant :: State -> Quadrant -> State
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

-- cellLens:: Lens' Quadrant Cell
-- cellLens =

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

initialState :: State
initialState = State 0 (mkInitialQuadrant One) (mkInitialQuadrant Two) (mkInitialQuadrant Three) (mkInitialQuadrant Four)

isDone :: State -> Bool
isDone s = False

-- isDone (State _ (_ (a1 a2 a3) (b1 b2 b3) (c1 c2 c3)) (_ (a4 a5 a6) (b4 b5 b6) (c4 c5 c6)) (_ (d1 d2 d3) (e1 e2 e3) (f1 f2 f3)) (_ (d4 d5 d6) (e4 e5 e6) (f4 f5 f6)) )= False

verifyDone :: Command -> State -> State -> (Message, Maybe State)
verifyDone (Command _ _ Nothing) oldState newState =
  if isDone newState
    then ("You won", Nothing)
    else ("You can only omit quadrant rotation when you finish the game", Just oldState)
verifyDone _ _ newState =
  if isDone newState
    then ("You won", Nothing)
    else ("", Just newState)

getQuadrantId :: Command -> QuadrantId
getQuadrantId (Command r c _)
  | (r < 3) && c < 3 = One
  | r < 3 = Two
  | c < 3 = Three
  | otherwise = Four

getQuadrant :: State -> QuadrantId -> Quadrant
getQuadrant s qId
  | qId == One = _q1 s
  | qId == Two = _q2 s
  | qId == Three = _q3 s
  | qId == Four = _q4 s

applyRotation :: Command -> State -> State
applyRotation (Command _ _ Nothing) s = s
applyRotation (Command _ _ (Just (quadrantId, "l"))) s = setQuadrant s $ rotateLeft $ getQuadrant s quadrantId
applyRotation (Command _ _ (Just (quadrantId, "r"))) s = setQuadrant s $ rotateRight $ getQuadrant s quadrantId

getQuadrantLens :: QuadrantId -> Lens' State Quadrant
getQuadrantLens quadrantId
  | quadrantId == One = q1
  | quadrantId == Two = q2
  | quadrantId == Three = q3
  | quadrantId == Four = q4

applyCommand :: State -> Command -> Either Message State
applyCommand s c = nextState `fmap` applyRotation c `fmap` setQuadrant s `fmap` updateQuadrant quadrant row col cell
  where
    quadrantId = getQuadrantId c
    quadrant = getQuadrant s quadrantId
    row = (_rowIdx c `mod` 3) + 1
    col = (_colIdx c `mod` 3) + 1
    cell = currentlyPlaying s

--turn =

step :: State -> Command -> (Message, Maybe State)
step s c = case applyCommand s c of --verifyDone c `fmap`
  Left message -> (message, Just s)
  Right newState -> verifyDone c s newState

currentlyPlaying :: State -> Cell
currentlyPlaying s = case mod (_roundNumber s) 2 of
  0 -> Black
  1 -> White

prompt :: State -> Message
prompt (State _ q1 q2 q3 q4) =
  show "   1  2  3  4  5  6" ++ "\n a "
    ++ show (_row1 q1)
    ++ "|"
    ++ show (_row1 q2)
    ++ "\n b "
    ++ show (_row2 q1)
    ++ "|"
    ++ show (_row2 q2)
    ++ "\n c "
    ++ show (_row3 q1)
    ++ "|"
    ++ show (_row3 q2)
    ++ "\n d "
    ++ show (_row1 q3)
    ++ "|"
    ++ show (_row1 q4)
    ++ "\n e "
    ++ show (_row2 q3)
    ++ "|"
    ++ show (_row2 q4)
    ++ "\n f "
    ++ show (_row3 q3)
    ++ "|"
    ++ show (_row3 q4)
    ++ "\n\n"

main = loop $ Just initialState
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
