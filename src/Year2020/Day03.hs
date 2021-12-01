module Year2020.Day03 where

import Aoc.Grid (Grid, fromStringWith, fromTuple, inBounds, sizeX, (!), mapP)
import Aoc.Util (countMatches)
import Aoc.Vector (V2, mapX, origin)
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (asciiGrid, Parser)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Text.Megaparsec.Char (char)
import Aoc.Puzzle (Puzzle, mkPuzzle)

data Tile = Free | Tree deriving (Eq, Show)

type Position = V2 Int

type Map = Grid Tile

part1 :: Puzzle Map Int
part1 = mkPuzzle grid $ solveSlope $ fromTuple (3, 1)

part2 :: Puzzle Map Int
part2 = mkPuzzle grid solvePart2

solveSlope :: V2 Int -> Map -> Int
solveSlope slope grid =
  let gridX = sizeX grid
      positions = takeWhile (inBounds grid) $ iterate (step gridX slope) origin
   in countMatches (checkTree grid) positions

solvePart2 :: Map -> Int
solvePart2 grid =
  let slopes = fromTuple <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
   in product $ (`solveSlope` grid) <$> slopes

checkTree :: Map -> Position -> Bool
checkTree grid pos = grid ! pos == Tree

step :: Int -> V2 Int -> Position -> Position
step gridX dr r = mapX (`mod` gridX) $ r + dr

grid :: Parser Map
grid = asciiGrid parseTile

parseTile :: Parser Tile
parseTile = (char '.' $> Free) <|> (char '#' $> Tree) <|> fail "Invalid tile" 