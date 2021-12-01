module Year2020.Day11 where

import Aoc.Grid (Grid, fromStringWith, imapP, withBorder, (!), mapP)
import Aoc.Util (countMatches, fixpoint, hasAtLeast)
import Aoc.Vector (V2 (V2))
import Data.Maybe (catMaybes)
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (asciiGrid, Parser)
import Data.Char (isSeparator)
import Text.Megaparsec.Char (char)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Aoc.Puzzle (Puzzle, mkPuzzle)

type Map = Grid Tile

data Tile = Border | Floor | Seat | Person deriving (Eq)

type StateFunction = Map -> Position -> Tile

type TilesFunction = Map -> Position -> [Tile]

type Position = V2 Int

type Direction = V2 Int

part1 :: Puzzle Map Int
part1 = mkPuzzle grid solvePart1

part2 :: Puzzle Map Int
part2 = mkPuzzle grid solvePart2

solvePart1 :: Map -> Int
solvePart1 = countMatches (== Person) . fixpoint (step $ nextState adjacentTiles 4)

solvePart2 :: Map -> Int
solvePart2 = countMatches (== Person) . fixpoint (step $ nextState visibleTiles 5)

step :: StateFunction -> Map -> Map
step f m = imapP (\k _ -> f m k) m

nextState :: TilesFunction -> Int -> StateFunction
nextState f n m p =
  let tile = m ! p
      others = f m p
   in case tile of
        Seat -> if Person `elem` others then Seat else Person
        Person -> if hasAtLeast n (== Person) others then Seat else Person
        Floor -> Floor
        Border -> Border

visibleTiles :: TilesFunction
visibleTiles m p = catMaybes [visibleTile m d p | d <- directions]

visibleTile :: Map -> Direction -> Position -> Maybe Tile
visibleTile m d p =
  let rayStep = p + d
   in case m ! rayStep of
        Floor -> visibleTile m d rayStep
        Person -> Just Person
        Seat -> Just Seat
        Border -> Nothing

adjacentTiles :: TilesFunction
adjacentTiles m p =
  let adjs = adjacents p
   in (m !) <$> adjs

adjacents :: Position -> [Position]
adjacents r = [r + dr | dr <- directions]

directions :: [Position]
directions = [V2 dx dy | dx <- [-1 .. 1], dy <- [-1 .. 1], dy /= 0 || dx /= 0]

grid :: Parser Map
grid = asciiGrid parseTile

parseTile :: Parser Tile
parseTile = 
  char '#' $> Person
  <|> char 'L' $> Seat
  <|> char '.' $> Floor
  <|> fail "Invalid tile"