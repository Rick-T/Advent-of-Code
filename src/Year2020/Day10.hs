module Year2020.Day10 where

import Data.List (sort)
import Aoc.Parsers (Parser, integer)
import Text.Megaparsec.Char (newline)
import Control.Applicative.Combinators ( sepBy )
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (Puzzle, mkPuzzle)

part1 :: Puzzle [Int] Int
part1 = mkPuzzle inputP solvePart1

part2 :: Puzzle [Int] Int
part2 = mkPuzzle  inputP solvePart2

inputP :: Parser [Int]
inputP = integer `sepBy` newline

solvePart1 :: [Int] -> Int
solvePart1 l = go (0 : sort l) 0 0
  where
    go [] _ _ = 0
    go [_] d1 d3 = d1 * (d3 + 1)
    go (x : y : r) d1 d3
      | x + 1 == y = go (y : r) (d1 + 1) d3
      | x + 2 == y = go (y : r) d1 d3
      | x + 3 == y = go (y : r) d1 $ d3 + 1
      | otherwise = go [] d1 (d3 + 1)

solvePart2 :: [Int] -> Int
solvePart2 ls = go (1 : repeat 0) (sort ls) 0
  where
    go [] [] _ = 0
    go as [] _ = head as
    go as (l : ls) i =
      let diff = (l - i -1)
          s = sum $ take (3 - diff) as
          as' = foldr (:) as (s : replicate diff 0)
       in go as' ls l