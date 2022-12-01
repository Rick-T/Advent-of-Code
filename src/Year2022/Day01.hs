module Year2022.Day01 where

import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (countMatches)
import Data.List (sort, tails)
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Char (newline)

type Inventory = [Int]

part1 :: Puzzle [Inventory] Int
part1 = mkPuzzle input solvePart1

part2 :: Puzzle [Inventory] Int
part2 = mkPuzzle input solvePart2

solvePart1 :: [Inventory] -> Int
solvePart1 = maximum . fmap sum

solvePart2 :: [Inventory] -> Int
solvePart2 = sum . take 3 . reverse . sort . fmap sum

input :: Parser [Inventory]
input = inventory `sepBy` newline

inventory :: Parser [Int]
inventory = integer `sepEndBy` newline