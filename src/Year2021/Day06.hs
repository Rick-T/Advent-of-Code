module Year2021.Day06 where

import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (counter, iterateN)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

part1 :: Puzzle (HashMap Int Int) Int
part1 = mkPuzzle input $ solve 80

part2 :: Puzzle (HashMap Int Int) Int
part2 = mkPuzzle input $ solve 256

solve :: Int -> HashMap Int Int -> Int
solve i = sum . M.elems . iterateN i nextDay

nextDay :: HashMap Int Int -> HashMap Int Int
nextDay = M.foldrWithKey updateMap M.empty

updateMap :: Int -> Int -> HashMap Int Int -> HashMap Int Int
updateMap 0 v = M.insertWith (+) 6 v . M.insert 8 v
updateMap i v = M.insert (i -1) v

input :: Parser (HashMap Int Int)
input = counter <$> integer `sepBy` char ','