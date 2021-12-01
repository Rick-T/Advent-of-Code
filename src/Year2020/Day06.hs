module Year2020.Day06 where

import Aoc.Input (parsePuzzleInput)
import Data.Set (Set, insert, fromList)
import Aoc.Parsers (Parser)
import Text.Megaparsec.Char (letterChar, newline)
import Text.Megaparsec (many, sepEndBy, sepBy, some)
import Aoc.Puzzle (Puzzle, mkPuzzle)

type Questions = Set Char

part1 :: Puzzle [Set String] Int
part1 = mkPuzzle questionaires $ solve any

part2 :: Puzzle [Set String] Int
part2 = mkPuzzle questionaires $ solve all

solve :: ((String -> Bool) -> Set String -> Bool) -> [Set String] -> Int
solve f = sum . fmap (countAnswers f)

countAnswers :: ((String -> Bool) -> Set String -> Bool) -> Set String -> Int
countAnswers f s = length [c | c <- ['a' .. 'z'], f (elem c) s]

questionaires :: Parser [Set String]
questionaires = questionaire `sepBy` newline

questionaire :: Parser (Set String)
questionaire = fromList <$> question `sepEndBy` newline

question :: Parser String
question = some letterChar