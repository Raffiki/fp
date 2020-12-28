module Parser where

import Control.Monad.Trans.Except
import Data.Either.Combinators
import Errors
import Files
import Lib
import System.IO
import Text.ParserCombinators.Parsec

colour :: GenParser Char st Player
colour = (char 'B' >> return Black) <|> (char 'W' >> return White)

row :: GenParser Char st Int
row =
  (char 'a' >> return 0)
    <|> (char 'b' >> return 1)
    <|> (char 'c' >> return 2)
    <|> (char 'd' >> return 3)
    <|> (char 'e' >> return 4)
    <|> (char 'f' >> return 5)

column :: GenParser Char st Int
column =
  (char '1' >> return 0)
    <|> (char '2' >> return 1)
    <|> (char '3' >> return 2)
    <|> (char '4' >> return 3)
    <|> (char '5' >> return 4)
    <|> (char '6' >> return 5)

quadrantId :: GenParser Char st QuadrantId
quadrantId =
  try (string "IV" >> return Four)
    <|> try (string "III" >> return Three)
    <|> try (string "II" >> return Two)
    <|> (string "I" >> return One)

rotation :: GenParser Char st Rotation
rotation =
  (char 'L' >> return L)
    <|> (char 'R' >> return R)

whitespace :: GenParser Char st String
whitespace = many (oneOf [' ', '\n'])

header :: GenParser Char st (Player, Player)
header = do
  humanPlayer <- colour
  whitespace
  _ <- char ';'
  whitespace
  startingPlayer <- colour
  whitespace
  _ <- char ';'
  whitespace
  return (humanPlayer, startingPlayer)

--ROW COLUMN QUADRANT ROT
move :: GenParser Char st NormalMove
move = do
  r <- row
  whitespace
  c <- column
  whitespace
  q <- quadrantId
  whitespace
  rot <- rotation
  whitespace
  return $ NormalMove (Position (r, c)) q rot

endMove :: GenParser Char st MoveSequence
endMove = do
  r <- row
  whitespace
  c <- column
  whitespace
  eof
  return $ End $ Position (r, c)

moves :: GenParser Char st MoveSequence
moves = try next <|> endMove
  where
    next = do
      first <- move
      next <- remainingMoves
      return (first :> next)

remainingMoves :: GenParser Char st MoveSequence
remainingMoves = next <|> return Empty
  where
    next = do
      whitespace
      char ','
      whitespace
      moves

gameFile :: GenParser Char st (Player, Player, MoveSequence)
gameFile = do
  (humanPlayer, startingPlayer) <- header
  ms <- moves
  eof
  return (humanPlayer, startingPlayer, ms)

parseFile :: String -> Either AppError (Player, Player, MoveSequence)
parseFile s = case parse gameFile "(unknown)" s of
  Left err -> Left $ AppParseError $ show err
  Right res -> Right res

printMoves :: MoveSequence -> String
printMoves (a :> Empty) = show a
printMoves (End p) = show p
printMoves (a :> as) = show a ++ ",\n" ++ printMoves as

printGame :: (Player, Player, MoveSequence) -> String
printGame (humanPlayer, startingPlayer, moves) =
  show humanPlayer ++ ";\n"
    ++ show startingPlayer
    ++ ";\n"
    ++ printMoves moves

writeToFile :: (Player, Player, MoveSequence) -> IO ()
writeToFile g = writeFile "game.ptg" $ printGame g

readFromFile :: IO (Either AppError (Player, Player, MoveSequence))
readFromFile = runExceptT $ do
  content <- ExceptT $ readFile' "game.ptg" (MissingFileError "cannot find game.ptg file")
  ExceptT $ return $ parseFile content

writeToFile2 :: String -> IO ()
writeToFile2 s = case parseFile s of
  Right g -> writeToFile g

printToConsole :: String -> Either AppError String
printToConsole s = mapRight printGame $ parseFile s

--parseFile "B; W; a1Ir"