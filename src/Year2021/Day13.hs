{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day13 where

import Aoc.Grid (Grid (Grid), fromList, imapP, inBounds, parseGrid, toList, (!))
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (fst3, median)
import Aoc.Vector (V2 (..), east, north, northEast, northWest, south, southEast, southWest, west)
import Control.Monad.State.Strict (MonadState (get), State, modify, replicateM, runState)
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (group, intercalate, partition, scanl', sort)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Search.BFS.Hashable (bfs, fromSeparate, noAccumulator, runSearch, searchAll, simple)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Internal.Read (IParser (runP))
import Language.Haskell.TH (Extension (OverloadedStrings))
import Text.Megaparsec (many, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)

data Direction = X | Y deriving (Show)

data Fold = Fold Direction Int deriving (Show)

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = M.size . (!! 1) . uncurry (scanl' fold)

solvePart2 = prettyPrint . last . uncurry (scanl' fold)

prettyPrint points =
  let (xMax, yMax) = bounds points
   in intercalate "\n" [[if M.member (V2 x y) points then '#' else '.' | x <- [0 .. xMax]] | y <- [0 .. yMax]]

fold points (Fold X x) =
  let (toKeep, toFlip) = partition ((< x) . _x . fst) $ M.toList points
   in M.union (M.fromList toKeep) (M.fromList $ (\(V2 a b, p) -> (V2 (2 * x - a) b, p)) <$> toFlip)
fold points (Fold Y y) =
  let (toKeep, toFlip) = partition ((< y) . _y . fst) $ M.toList points
   in M.unionWith (&&) (M.fromList toKeep) (M.fromList $ (\(V2 a b, p) -> (V2 a (2 * y - b), p)) <$> toFlip)

paper points = M.fromList $ zip points $ repeat True

bounds points = (maximum $ _x <$> M.keys points, maximum $ _y <$> M.keys points)

input = (,) <$> (paper <$> dots <* newline) <*> instructions

instructions = instruction `sepBy` newline

instruction = Fold <$> (string "fold along " *> direction) <*> (char '=' *> integer)

direction = (char 'x' $> X) <|> (char 'y' $> Y)

dots = dot `sepEndBy` newline

dot = V2 <$> (integer <* char ',') <*> integer