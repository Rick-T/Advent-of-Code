{-# LANGUAGE BangPatterns #-}

module Year2020.Day15 where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (State, evalState, get, gets, put)
import Data.Foldable (foldlM)
import Data.IntMap (IntMap)
import Data.IntMap.Strict as M (fromList, insert, lookup)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V
import Aoc.Parsers ( Parser, integer )
import Control.Applicative.Combinators (sepBy)
import Text.Megaparsec.Char (char)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (mkPuzzle, Puzzle)

type Map = IntMap Int

data GameStateT = GameState {getMap :: !Map, getNumber :: !Int, getCounter :: !Int}

type GameState = State GameStateT

part1 :: Puzzle [Int] Int
part1 = mkPuzzle inputP $ solveV 2020

part2 :: Puzzle [Int] Int
part2 = mkPuzzle inputP $ solveV 30000000

inputP :: Parser [Int]
inputP = integer `sepBy` char ','

solveV :: Int -> [Int] -> Int
solveV steps initial = runST $ do
  vector <- V.new steps
  mapM_ (uncurry $ V.write vector) $ zip (init initial) [1 ..]
  foldlM
    (stepV vector)
    (last initial)
    [length initial .. steps - 1]

stepV :: MVector s Int -> Int -> Int -> ST s Int
stepV vector num counter = do
  lastCount <- V.read vector num
  V.write vector num counter
  if lastCount == 0
    then return 0
    else return $ counter - lastCount

solve :: Int -> [Int] -> Int
solve steps initial = evalState (forM_ [length initial .. steps -1] (const step) *> gets getNumber) (start initial)

start :: [Int] -> GameStateT
start xs =
  let m = M.fromList $ zip (init xs) [1 ..]
   in GameState m (last xs) (length xs)

step :: GameState ()
step = do
  GameState map num counter <- get
  let !m' = insert num counter map
  let !n' = case M.lookup num map of
        Nothing -> 0
        Just lastCount -> counter - lastCount
  put (GameState m' n' (counter + 1))
