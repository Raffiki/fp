module Main where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Lib
import Text.Regex.Posix ((=~))

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

main :: IO ()
main =
  do
    startingPlayer <- randomPlayer
    loop $ Just $ initialBoard startingPlayer
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

step :: Board -> Command -> (Message, Maybe Board)
step b c = case applyCommand b c of
  Left message -> (message, Just b)
  Right newBoard -> verifyDone c b newBoard

verifyDone :: Command -> Board -> Board -> (Message, Maybe Board)
verifyDone (Command _ _ Nothing) oldBoard newBoard =
  if _player oldBoard `elem` getWinners newBoard
    then ("You won", Nothing)
    else ("You can only omit quadrant rotation when *you* win the game", Just oldBoard)
verifyDone _ _ newBoard = case getWinners newBoard of
  [_, _] -> ("You both won", Nothing)
  [winner] -> (show winner ++ " won", Nothing)
  _ -> ("", Just newBoard)