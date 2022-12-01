{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day21 where

import qualified Aoc.Grid as G
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (counter, firstEqual, fromBinary, fst3, iterateN, median, minMax)
import Control.Applicative (Alternative (empty), liftA3)
import Control.Arrow ((<<<), (<<^), (^<<))
import Control.Monad.State.Strict (MonadState (get), State, evalState, execState, forM, modify, replicateM, runState, (<=<))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl', maximumBy)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap, keys, mapWithKey, (!?))
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (foldl1', group, intercalate, partition, permutations, scanl', sort, transpose)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Search.BFS.Hashable (bfs, fromSeparate, fromSeparateM, listAccumulator, noAccumulator, runSearch, runSearchT, searchAll, searchAllM, simple, simpleM)
import Data.Search.Simple (dijkstraM)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Text.Internal.Read (IParser (runP))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Language.Haskell.TH (Extension (OverloadedStrings))
import Text.Megaparsec (count, many, parseMaybe, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)
import Year2020.Day22 (score)

data PlayerState = PlayerState
  { _pos :: Int,
    _score :: Int
  }
  deriving (Eq, Generic, Hashable, Show)

data GameState = GameState
  { _p1 :: PlayerState,
    _p2 :: PlayerState,
    _p1Active :: Bool
  }
  deriving (Eq, Generic, Hashable)

type Game = State GameState

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = uncurry result . runGame 1000 dieRolls . (0,)

solvePart2 gamestate = uncurry max $ evalState (runGame3 gamestate) mempty

result :: Int -> GameState -> Int
result rolls (GameState (PlayerState _ s1) (PlayerState _ s2) _) = 3 * rolls * min s1 s2

dieRolls = [6, 15 ..]

toUniverses 3 = 1
toUniverses 4 = 3
toUniverses 5 = 6
toUniverses 6 = 7
toUniverses 7 = 6
toUniverses 8 = 3
toUniverses 9 = 1

runGame3 :: GameState -> State (HashMap GameState (Int, Int)) (Int, Int)
runGame3 gamestate
  | gameOver 21 gamestate = if _p1Active gamestate then return (0, 1) else return (1, 0)
  | otherwise = do
    cache <- get
    flip
      fromMaybe
      (return <$> (cache !? gamestate))
      $ do
        nextStates <- sequence [(\(a, b) -> (universes * a, universes * b)) <$> runGame3 next | roll <- [3 .. 9], let universes = toUniverses roll, let next = takeTurn roll gamestate]
        let result = foldl1' (\(a, b) (x, y) -> (a + x, b + y)) nextStates
        modify (M.insert gamestate result)
        return result

runGame2 target gamestates = do
  state <- gamestates
  if gameOver target state
    then return state
    else do
      runGame2 target $ (`takeTurn` state) <$> [3 .. 9]

runGame :: Int -> [Int] -> (Int, GameState) -> (Int, GameState)
runGame target (die : rest) (rolls, state) = if gameOver target state then (rolls, state) else runGame target rest (rolls + 1, takeTurn die state)

gameOver :: Int -> GameState -> Bool
gameOver target (GameState (PlayerState _ score) _ False) = score >= target
gameOver target (GameState _ (PlayerState _ score) True) = score >= target

takeTurn :: Int -> GameState -> GameState
takeTurn die s@(GameState p1 p2 p1Active) =
  let s' = s {_p1Active = not p1Active}
   in if p1Active then s' {_p1 = move die p1} else s' {_p2 = move die p2}

move :: Int -> PlayerState -> PlayerState
move die (PlayerState pos score) =
  let newPos = ((pos - 1 + die) `mod` 10) + 1
   in PlayerState newPos $ score + newPos

input = initialState <$> (startPos <* newline) <*> startPos

initialState p1Pos p2Pos = GameState (PlayerState p1Pos 0) (PlayerState p2Pos 0) True

startPos = string "Player " *> integer *> string " starting position: " *> integer