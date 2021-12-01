module Year2021.Day01 where
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, integer)
import Text.Megaparsec (many, sepBy, sepEndBy, anySingle)
import Text.Megaparsec.Char (newline)
import Data.Text (Text)
import Aoc.Puzzle (Puzzle, mkPuzzle)

part1 :: Puzzle [Int] Int
part1 = mkPuzzle numbers solvePart1

part2 :: Puzzle [Int] Int
part2 = mkPuzzle numbers solvePart2

solvePart1 :: [Int] -> Int
solvePart1 = countIncreases

solvePart2 :: [Int] -> Int
solvePart2 = solvePart1 . slideWindows 3

countIncreases :: [Int] -> Int 
countIncreases = length . filter (> 0) . (zipWith (-) <$> tail <*> id)

slideWindows :: Int -> [Int] -> [Int]
slideWindows windowSize = foldr (zipWith (+)) (repeat 0) . take windowSize . iterate tail

numbers :: Parser [Int]
numbers = integer `sepBy` newline