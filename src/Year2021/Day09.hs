module Year2021.Day09 where

import Aoc.Grid (Grid, inBounds, toList, (!))
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (fst3)
import Aoc.Vector (V2, east, north, south, west)
import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Search.BFS.Hashable (bfs, fromSeparate, noAccumulator, runSearch, searchAll, simple)
import Text.Megaparsec.Char (digitChar)

part1 :: Puzzle (Grid Int) Int
part1 = mkPuzzle input solvePart1

part2 :: Puzzle (Grid Int) Int
part2 = mkPuzzle input solvePart2

solvePart1 :: Grid Int -> Int
solvePart1 = sum . fmap (+ 1) . liftM2 fmap (!) lowPoints

solvePart2 :: Grid Int -> Int
solvePart2 = product . take 3 . sortOn negate . fmap length . liftM2 fmap basin lowPoints

basin :: Grid Int -> V2 Int -> [V2 Int]
basin grid = fmap fst3 . runSearch (searchAll (simple $ const True)) bfs noAccumulator (fromSeparate (neighbours grid) (\_ b -> if grid ! b /= 9 then Just () else Nothing))

lowPoints :: Grid Int -> [V2 Int]
lowPoints grid = filter (isLowPoint grid) $ fst <$> toList grid

isLowPoint :: Grid Int -> V2 Int -> Bool
isLowPoint grid p = all ((grid ! p) <) (neighbourValues grid p)

neighbourValues :: Grid Int -> V2 Int -> [Int]
neighbourValues grid p = (grid !) <$> neighbours grid p

neighbours :: Grid Int -> V2 Int -> [V2 Int]
neighbours grid = filter (inBounds grid) . traverse (+) [north, south, east, west]

input :: Parser (Grid Int)
input = asciiGrid (digitToInt <$> digitChar)