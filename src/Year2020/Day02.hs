module Year2020.Day02 where

import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, positiveInt)
import Aoc.Util (countMatches)
import Text.Megaparsec (some, sepBy)
import Text.Megaparsec.Char (char, letterChar, newline)
import Aoc.Puzzle (mkPuzzle, Puzzle)

data Policy = Policy Int Int Char deriving (Show)

data DbEntry = DbEntry Policy Password deriving (Show)

type Password = String

part1 :: Puzzle [DbEntry] Int
part1 = mkPuzzle dbEntriesParser solvePart1

part2 :: Puzzle [DbEntry] Int
part2 = mkPuzzle dbEntriesParser solvePart2

solvePart1 :: [DbEntry] -> Int
solvePart1 = countMatches checkDbEntry

solvePart2 :: [DbEntry] -> Int
solvePart2 = countMatches checkDbEntry'

checkDbEntry :: DbEntry -> Bool
checkDbEntry (DbEntry (Policy minCount maxCount letter) password) =
  let letterCount = occurences letter password
   in letterCount >= minCount && letterCount <= maxCount

checkDbEntry' :: DbEntry -> Bool
checkDbEntry' (DbEntry (Policy pos1 pos2 letter) password) =
  let first = password !! (pos1 - 1)
      second = password !! (pos2 - 1)
   in (first == letter) /= (second == letter)

occurences :: Char -> Password -> Int
occurences letter = countMatches (== letter)

dbEntriesParser :: Parser [DbEntry]
dbEntriesParser = dbEntryParser `sepBy` newline

dbEntryParser :: Parser DbEntry
dbEntryParser = do
  minCount <- positiveInt
  _ <- char '-'
  maxCount <- positiveInt
  _ <- char ' '
  letter <- letterChar
  _ <- char ':' *> char ' '
  password <- some letterChar
  return $ DbEntry (Policy minCount maxCount letter) password
