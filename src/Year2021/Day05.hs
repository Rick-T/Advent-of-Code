{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day05 where

import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Vector (V2 (V2), maxNorm, (.*))
import Control.Monad.Combinators (sepBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Text.Megaparsec.Char (char, newline, string)

type Point = V2 Int

data LineDef = LineDef
  { _start :: Point,
    _end :: Point
  }

part1 :: Puzzle [LineDef] Int
part1 = mkPuzzle input (solve . filter orthogonal)

part2 :: Puzzle [LineDef] Int
part2 = mkPuzzle input solve

solve :: [LineDef] -> Int
solve = M.size . M.filter (> 1) . buildMap

buildMap :: [LineDef] -> HashMap Point Int
buildMap = foldr (uncurry (M.insertWith (+))) M.empty . (`zip` repeat 1) . concatMap toLine

toLine :: LineDef -> [Point]
toLine (LineDef x y) =
  let delta = y - x
      direction = signum delta
      distance = maxNorm delta
   in [x + i .* direction | i <- [0 .. distance]]

orthogonal :: LineDef -> Bool
orthogonal (LineDef (V2 a b) (V2 x y)) = a == x || b == y

input :: Parser [LineDef]
input = lineDef `sepBy` newline

lineDef :: Parser LineDef
lineDef = LineDef <$> (coordinate <* string " -> ") <*> coordinate

coordinate :: Parser Point
coordinate = V2 <$> (integer <* char ',') <*> integer