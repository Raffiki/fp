module Parser where

import Lib
import Text.ParserCombinators.Parsec

-- file = colour ';' colour ';'

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
  try (string "VI" >> return One)
    <|> try (string "III" >> return One)
    <|> try (string "II" >> return One)
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
  _ <- whitespace
  _ <- char ';'
  whitespace
  startingPlayer <- colour
  _ <- whitespace
  _ <- char ';'
  _ <- whitespace
  return (humanPlayer, startingPlayer)

--ROW COLUMN QUADRANT ROT
move :: GenParser Char st Move
move = do
  r <- row
  _ <- whitespace
  c <- column
  _ <- whitespace
  q <- quadrantId
  _ <- whitespace
  rot <- rotation
  _ <- whitespace
  return $ Move (r, c) q rot

moves :: GenParser Char st [Move]
moves = do
  first <- move
  next <- remainingMoves
  return (first : next)

remainingMoves :: GenParser Char st [Move]
remainingMoves = next <|> return []
  where
    next = do
      whitespace
      char ','
      whitespace
      moves

gameFile :: GenParser Char st (Player, Player, [Move])
gameFile = do
  (humanPlayer, startingPlayer) <- header
  ms <- moves
  eof
  return (humanPlayer, startingPlayer, ms)

parseFile :: String -> Either ParseError (Player, Player, [Move])
parseFile = parse gameFile "(unknown)"

--parseFile "B; W; a1Ir"