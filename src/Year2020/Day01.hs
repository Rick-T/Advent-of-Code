module Year2020.Day01 where
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, integer)
import Text.Megaparsec (many, sepBy)
import Text.Megaparsec.Char (newline)
import Aoc.Puzzle (mkPuzzle, Puzzle)

part1 :: Puzzle [Int] Int
part1 = mkPuzzle input solvePart1

part2 :: Puzzle [Int] Int
part2 = mkPuzzle input solvePart2

solvePart1 :: [Int] -> Int
solvePart1 input = head [x * y | x <- input, y <- input, x + y == 2020]

solvePart2 :: [Int] -> Int
solvePart2 input = head [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]

input :: Parser [Int]
input = integer `sepBy` newline