module Year2021.Day06 where

import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (iterateN)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

part1 :: Puzzle (IntMap Int) Int
part1 = mkPuzzle input $ solve 80

part2 :: Puzzle (IntMap Int) Int
part2 = mkPuzzle input $ solve 256

solve :: Int -> IntMap Int -> Int
solve i = sum . M.elems . iterateN i nextDay

nextDay :: IntMap Int -> IntMap Int
nextDay = M.foldrWithKey updateMap M.empty

updateMap :: Int -> Int -> IntMap Int -> IntMap Int
updateMap 0 v = M.insertWith (+) 6 v . M.insert 8 v
updateMap i v = M.insert (i -1) v

input :: Parser (IntMap Int)
input = M.fromListWith (+) . (`zip` repeat 1) <$> integer `sepBy` char ','