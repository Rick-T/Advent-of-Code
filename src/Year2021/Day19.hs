{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day19 where

import Aoc.Grid (Grid (Grid), fromList, imapP, inBounds, parseGrid, toList, (!))
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (counter, fst3, median)
import Control.Applicative (liftA3)
import Control.Arrow ((<<<), (<<^), (^<<))
import Control.Monad.State.Strict (MonadState (get), State, modify, replicateM, runState, (<=<))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl', maximumBy)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (group, intercalate, partition, permutations, scanl', sort, transpose)
import Data.Maybe (catMaybes, fromJust, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Search.BFS.Hashable (bfs, fromSeparate, noAccumulator, runSearch, searchAll, simple)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Text.Internal.Read (IParser (runP))
import GHC.Generics (Generic)
import Language.Haskell.TH (Extension (OverloadedStrings))
import Text.Megaparsec (many, parseMaybe, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)

data V3 a = V3
  { _x :: a,
    _y :: a,
    _z :: a
  }
  deriving (Eq, Ord, Show, Hashable, Generic)

instance (Num a) => (Num (V3 a)) where
  (V3 a b c) + (V3 x y z) = V3 (a + x) (b + y) (c + z)
  negate (V3 a b c) = V3 (negate a) (negate b) (negate c)

data Scanner = Scanner
  { _id :: Int,
    _beacons :: [V3 Int]
  }
  deriving (Show, Eq)

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = S.size . locateBeacons . locateScanners

solvePart2 scanners = maximum [manhattanDistance (p1 - p2) | let ps = M.keys $ locateScanners scanners, p1 <- ps, p2 <- ps]

manhattanDistance (V3 a b c) = abs a + abs b + abs c

locateBeacons :: HashMap (V3 Int) Scanner -> Set (V3 Int)
locateBeacons m = S.fromList $ concatMap (\(p, Scanner _ ps) -> (+ p) <$> ps) $ M.toList m

locateScanners :: [Scanner] -> HashMap (V3 Int) Scanner
locateScanners (scanner0 : rest) =
  let go final needChecking [] = M.union final needChecking
      go final needChecking scanners =
        let found = M.fromList [(p0 + offset, oriented) | (p0, s0) <- M.toList (needChecking :: HashMap (V3 Int) Scanner), s1 <- scanners, Just (oriented, offset) <- [overlap' s0 s1]]
            final' = M.union final needChecking
            scanners' = filter ((`notElem` (_id <$> M.elems found)) . _id) scanners
         in go final' found scanners'
   in go M.empty (M.singleton (V3 0 0 (0 :: Int)) scanner0) rest

overlap' :: Scanner -> Scanner -> Maybe (Scanner, V3 Int)
overlap' s1 s2 = case [(s, offset) | s <- orientations s2, Just offset <- [overlap s1 s]] of
  [result] -> Just result
  _ -> Nothing

overlap :: Scanner -> Scanner -> Maybe (V3 Int)
overlap (Scanner _ b1) (Scanner _ b2) =
  let (difference, count) = maximumBy (comparing snd) $ M.toList $ counter $ [a - b | a <- b1, b <- b2]
   in if count >= 12 then Just difference else Nothing

orientations :: Scanner -> [Scanner]
orientations (Scanner id beacons) = Scanner id . sort <$> transpose [[V3 x y z | [u, v, w] <- permutations [a, b, c], x <- [- u, u], y <- [- v, v], z <- [- w, w]] | (V3 a b c) <- beacons]

header = string "--- scanner " *> integer <* string " ---"

beacon = V3 <$> integer <* char ',' <*> integer <* char ',' <*> integer

scanner = Scanner <$> header <* newline <*> beacon `sepEndBy` newline

input = scanner `sepBy` newline