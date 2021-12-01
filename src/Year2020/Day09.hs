module Year2020.Day09 where

import Aoc.Util (minMax)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, integer)
import Text.Megaparsec.Char (newline)
import Control.Applicative.Combinators
import Text.Megaparsec (parse)
import Aoc.Puzzle (Puzzle, mkPuzzle)

part1 :: Puzzle (Vector Int) Int
part1 = mkPuzzle vector solvePart1

part2 :: Puzzle (Vector Int) Int
part2 = mkPuzzle vector solvePart2

solvePart1 :: Vector Int -> Int
solvePart1 xs =
  let x = xs ! 25
      pairSums = [x + y | x <- (xs !) <$> [0 .. 24], y <- (xs !) <$> [0 .. 24], x /= y]
   in if x `elem` pairSums then solvePart1 (V.tail xs) else x

solvePart2 :: Vector Int -> Int
solvePart2 xs =
  let target = solvePart1 xs
      (x, y) = minMax $ contiguousTo target xs
   in x + y

contiguousTo :: Int -> Vector Int -> Vector Int
contiguousTo target xs = go 0 0 0
  where
    go low high acc
      | acc < target = go low (high + 1) (acc + xs ! high)
      | acc > target = go (low + 1) high (acc - xs ! low)
      | otherwise = V.slice low (high - low) xs

vector :: Parser (Vector Int)
vector = V.fromList <$> integer `sepBy` newline