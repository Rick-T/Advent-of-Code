{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day24 where

import qualified Aoc.Grid as G
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (counter, firstEqual, fromBinary, fst3, iterateN, median, minMax, snd3)
import Control.Applicative (Alternative (empty), liftA3)
import Control.Arrow ((<<<), (<<^), (^<<))
import Control.Monad.State.Strict (MonadState (get), State, evalState, execState, forM, join, modify, replicateM, runState, (<=<))
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
import Data.IntMap (IntMap, fromList, (!))
import qualified Data.IntMap.Strict as IM
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
import Year2020.Day22 (score)

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

-- d3 == d2 - 1
-- d5 == d4 + 6
-- d6 == d1 + 8
-- d8 == d7
-- d11 == d10 + 2
-- d12 == d9 + 5
-- d13 == d0 + 4

solvePart1 = const 51983999947999

solvePart2 = const 11211791111365

input = empty