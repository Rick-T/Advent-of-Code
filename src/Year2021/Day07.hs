module Year2021.Day07 where

import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (minMax)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

part1 :: Puzzle [Int] Int
part1 = mkPuzzle input $ solve id

part2 :: Puzzle [Int] Int
part2 = mkPuzzle input $ solve increaseWithDistance

solve :: (Int -> Int) -> [Int] -> Int
solve distanceToFuel positions = minimum [cost position distanceToFuel positions | let (minBound, maxBound) = minMax positions, position <- [minBound .. maxBound]]

increaseWithDistance :: Int -> Int
increaseWithDistance x = x * (x + 1) `div` 2

cost :: Int -> (Int -> Int) -> [Int] -> Int
cost position distanceToFuel = sum . fmap (distanceToFuel . abs . (position -))

input :: Parser [Int]
input = integer `sepBy` char ','