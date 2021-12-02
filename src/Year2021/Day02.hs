{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day02 where

import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (countMatches)
import Aoc.Vector (V2 (V2), east, (*.), (.*))
import Data.Functor (($>))
import Data.List (foldl', tails)
import Text.Megaparsec (sepBy, (<|>))
import Text.Megaparsec.Char (newline, space, string)

data State = State {_aim :: Int, _pos :: V2 Int}

part1 :: Puzzle [V2 Int] Int
part1 = mkPuzzle commands solvePart1

part2 :: Puzzle [V2 Int] Int
part2 = mkPuzzle commands solvePart2

solvePart1 :: [V2 Int] -> Int
solvePart1 = product . sum

solvePart2 :: [V2 Int] -> Int
solvePart2 = product . _pos . foldl' followCommand (State 0 0)

followCommand :: State -> V2 Int -> State
followCommand (State aim pos) (V2 dx da) = State (aim + da) $ pos + dx .* (east + aim .* down)

commands :: Parser [V2 Int]
commands = command `sepBy` newline

command :: Parser (V2 Int)
command = (*.) <$> (direction <* space) <*> integer

direction :: Parser (V2 Int)
direction =
  string "forward" $> east
    <|> string "down" $> down
    <|> string "up" $> up

down :: V2 Int
down = V2 0 1

up :: V2 Int
up = V2 0 (-1)