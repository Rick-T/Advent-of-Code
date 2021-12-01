{-# LANGUAGE LambdaCase #-}
module Year2020.Day05 where

import Aoc.Input (parsePuzzleInput)
import Data.List (foldl', sort)
import Aoc.Parsers (Parser, integer)
import Text.Megaparsec.Char (letterChar, newline)
import Control.Applicative ( Alternative(many) )
import Control.Applicative.Combinators (sepBy)
import Text.Megaparsec.Char.Lexer (lineFold)
import Aoc.Puzzle (Puzzle, mkPuzzle)

part1 :: Puzzle [Int] Int
part1 = mkPuzzle seatIds maximum

part2 :: Puzzle [Int] Int
part2 =  mkPuzzle seatIds solvePart2

seatIds :: Parser [Int]
seatIds = seatId `sepBy` newline

seatId :: Parser Int
seatId = foldl' (\a b -> 2 * a + b) 0 <$> many seatDigit

solvePart2 :: [Int] -> Int
solvePart2 = findSeat . sort

findSeat :: [Int] -> Int
findSeat [] = error "Dude where is my seat?"
findSeat [_] = error "Dude where is my seat?"
findSeat (x : y : r)
  | x + 1 == y = findSeat (y : r)
  | otherwise = x + 1

seatDigit :: Parser Int
seatDigit = letterChar >>= \case
    'F' -> return 0
    'B' -> return 1
    'L' -> return 0
    'R' -> return 1
    _ -> fail "Invalid char"