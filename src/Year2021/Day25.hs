{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day25 where

import Aoc.Grid (Grid (Grid), GridIndex (fromTuple, toTuple), bounds, imapP, (!))
import qualified Aoc.Grid as G
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (counter, firstEqual, fromBinary, fst3, iterateN, median, minMax, snd3)
import Aoc.Vector (V2 (V2), east, origin, south)
import Control.Applicative (Alternative (empty), liftA3)
import Control.Arrow ((<<<), (<<^), (^<<))
import Data.Bifunctor (Bifunctor (first, second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl', maximumBy)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap, keys, mapWithKey, (!?))
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (foldl1', group, intercalate, partition, permutations, scanl', sort, transpose)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Search.BFS.Hashable (bfs, fromCombined, fromSeparate, fromSeparateM, listAccumulator, noAccumulator, runSearch, runSearchT, search, searchAll, searchAllM, simple, simpleM)
import Data.Search.Simple (dijkstraM)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (pack)
import qualified Data.Text.IO as T
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal.Read (IParser (runP))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Language.Haskell.TH (Extension (OverloadedStrings))
import Text.Megaparsec (count, many, parseMaybe, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)
import Text.Printf (printf)

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

-- d3 == d2 - 1
-- d5 == d4 + 6
-- d6 == d1 + 8
-- d8 == d7
-- d11 == d10 + 2
-- d12 == d9 + 5
-- d13 == d0 + 4

solvePart1 grid =
  let steps = iterate step grid
      pairs = zip steps $ tail steps
   in 1 + length (takeWhile (uncurry (/=)) pairs)

solvePart2 = undefined

step grid = substep south $ substep east grid

substep direction grid = imapP (move grid direction) grid

move :: Grid (V2 Int) -> V2 Int -> V2 Int -> V2 Int -> V2 Int
move grid direction position cucumber
  | cucumber == direction && get grid (position + direction) == origin = origin
  | cucumber == origin && get grid (position - direction) == direction = direction
  | otherwise = cucumber

get :: GridIndex b => Grid a -> b -> a
get grid pos = grid ! wrap grid pos

wrap :: GridIndex b => Grid a -> b -> b
wrap grid pos =
  let (bX, bY) = bounds grid
      (x, y) = toTuple pos
   in fromTuple (x `mod` bX, y `mod` bY)

input = asciiGrid cucumber

cucumber :: Parser (V2 Int)
cucumber = char 'v' $> south <|> char '>' $> east <|> char '.' $> origin

put :: Grid (V2 Int) -> IO ()
put = putStr . G.prettyPrint (\x -> if x == east then '>' else if x == south then 'v' else '.')