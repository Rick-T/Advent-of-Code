{-# LANGUAGE TupleSections #-}

module Year2020.Day13 where

import Aoc.Parsers (Parser, integer)
import Control.Applicative (Alternative ((<|>)))
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Megaparsec (sepBy, try)
import Text.Megaparsec.Char (char, newline)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (Puzzle, mkPuzzle)

type Timestamp = Integer

type BusId = Integer

data Schedule = Schedule Timestamp [BusId]

part1 :: Puzzle Schedule Integer
part1 = mkPuzzle schedule solvePart1

part2 :: Puzzle [(BusId, Integer)] Integer 
part2 = mkPuzzle contest solvePart2

solvePart1 :: Schedule -> Integer
solvePart1 (Schedule timestamp busIds) = uncurry (*) $ minimum [(busId - timestamp `mod` busId, busId) | busId <- busIds]

solvePart2 :: [(BusId, Integer)] -> Integer
solvePart2 ((start, _) : rest) = go start rest
  where
    go _ [] = 0
    go inc ((interval, offset) : bs) =
      let steps = solveMod interval (inc, - offset)
       in steps + go (inc * interval) (fmap (+ steps) <$> bs)

solveMod :: Integer -> (Integer, BusId) -> Integer
solveMod m (interval, offset) = interval * ((offset * interval ^ (m -2)) `mod` m)

contest :: Parser [(Integer, BusId)]
contest = do
  _ <- integer
  _ <- newline
  catMaybes . zipWith (\a -> fmap (,a)) [0 ..] <$> busList

busList :: Parser [Maybe BusId]
busList = (try (char 'x' $> Nothing) <|> Just <$> integer) `sepBy` char ','

schedule :: Parser Schedule
schedule = do
  timestamp <- integer
  _ <- newline
  busIds <- catMaybes <$> busList
  return $ Schedule timestamp busIds