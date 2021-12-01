{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day24 where

import Aoc.Parsers (Parser)
import Aoc.Util (countMatches, iterateN)
import Aoc.Vector (V2 (V2), east, north, south, west)
import Control.Applicative.Combinators (many, sepBy, (<|>))
import Data.Functor (($>))
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Text.Megaparsec.Char (char, newline, string)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (mkPuzzle, Puzzle)

data Direction = E | SE | SW | W | NW | NE deriving (Enum)

type Position = V2 Int

type Floor = HashSet Position


part1 :: Puzzle Floor Int
part1 = mkPuzzle inputP $ solve 0

part2 :: Puzzle Floor Int
part2 = mkPuzzle inputP $ solve 100

solve :: Int -> Floor -> Int
solve iterations = S.size . iterateN iterations step

step :: Floor -> Floor
step old = foldr (update old) mempty $ S.fromList $ concatMap adjacents $ S.toList old

update :: Floor -> Position -> Floor -> Floor
update old pos
  | turnsBlack old pos = S.insert pos
  | otherwise = id

turnsBlack :: Floor -> Position -> Bool
turnsBlack m p =
  let isBlack = p `S.member` m
      blackNeighbours = countMatches (`S.member` m) $ adjacents p
   in isBlack && blackNeighbours == 1 || blackNeighbours == 2

adjacents :: Position -> [Position]
adjacents r = [r + dr | dr <- directions]

directions :: [Position]
directions = [toPosition dir | dir <- [E .. NE]]

inputP :: Parser Floor
inputP = foldr insertOrDelete mempty <$> positionP `sepBy` newline

insertOrDelete :: Position -> Floor -> Floor
insertOrDelete p s
  | p `S.member` s = S.delete p s
  | otherwise = S.insert p s

positionP :: Parser Position
positionP = sum . fmap toPosition <$> many directionP

toPosition :: Direction -> Position
toPosition E = 2 * east
toPosition W = 2 * west
toPosition SE = south + east
toPosition SW = south + west
toPosition NE = north + east
toPosition NW = north + west

directionP :: Parser Direction
directionP =
  string "se" $> SE
    <|> string "sw" $> SW
    <|> string "nw" $> NW
    <|> string "ne" $> NE
    <|> char 'e' $> E
    <|> char 'w' $> W