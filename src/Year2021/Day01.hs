module Year2021.Day01 where

import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (countMatches)
import Data.List (tails)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline)

part1 :: Puzzle [Int] Int
part1 = mkPuzzle numbers solvePart1

part2 :: Puzzle [Int] Int
part2 = mkPuzzle numbers solvePart2

solvePart1 :: [Int] -> Int
solvePart1 = countIncreases

solvePart2 :: [Int] -> Int
solvePart2 = solvePart1 . slideWindows 3

countIncreases :: [Int] -> Int
countIncreases = countMatches (< 0) . (zipWith (-) <*> tail)

slideWindows :: Int -> [Int] -> [Int]
slideWindows windowSize = foldr1 (zipWith (+)) . take windowSize . tails

numbers :: Parser [Int]
numbers = integer `sepBy` newline