{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day18 where

import Aoc.Grid (Grid (Grid), fromList, imapP, inBounds, parseGrid, toList, (!))
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (fst3, median)
import Aoc.Vector (V2 (..), east, north, northEast, northWest, south, southEast, southWest, west)
import Control.Arrow ((<<<), (<<^), (^<<))
import Control.Monad.State.Strict (MonadState (get), State, modify, replicateM, runState, (<=<))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (group, intercalate, partition, scanl', sort)
import Data.Maybe (catMaybes, fromJust, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Search.BFS.Hashable (bfs, fromSeparate, noAccumulator, runSearch, searchAll, simple)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Internal.Read (IParser (runP))
import Language.Haskell.TH (Extension (OverloadedStrings))
import Text.Megaparsec (many, parseMaybe, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)

data Number = Regular Int | Pair Number Number deriving (Eq)

type Path = [Either Number Number]

type Zipper = (Number, Path)

instance Semigroup Number where
  x <> y = reduce $ Pair x y

leftmost :: Zipper -> Zipper
leftmost zipper = maybe zipper leftmost (left zipper)

rightmost :: Zipper -> Zipper
rightmost zipper = maybe zipper rightmost (right zipper)

topmost :: Zipper -> Zipper
topmost zipper = maybe zipper topmost (up zipper)

left :: Zipper -> Maybe Zipper
left (Regular _, _) = Nothing
left (Pair a b, bs) = Just (a, Left b : bs)

right :: Zipper -> Maybe Zipper
right (Regular _, _) = Nothing
right (Pair a b, bs) = Just (b, Right a : bs)

up :: Zipper -> Maybe Zipper
up (_, []) = Nothing
up (x, Left y : bs) = Just (Pair x y, bs)
up (y, Right x : bs) = Just (Pair x y, bs)

isRight :: Zipper -> Bool
isRight (_, Right _ : _) = True
isRight _ = False

isLeft :: Zipper -> Bool
isLeft (_, Left _ : _) = True
isLeft _ = False

leftNeighbour :: Zipper -> Maybe Zipper
leftNeighbour zipper
  | isRight zipper = rightmost <$> (left =<< up zipper)
  | otherwise = leftNeighbour =<< up zipper

rightNeighbour :: Zipper -> Maybe Zipper
rightNeighbour zipper
  | isLeft zipper = leftmost <$> (right =<< up zipper)
  | otherwise = rightNeighbour =<< up zipper

findFirst :: (Zipper -> Bool) -> Zipper -> Maybe Zipper
findFirst f zipper
  | f zipper = Just zipper
  | otherwise = case findFirst f =<< left zipper of
    Nothing -> findFirst f =<< right zipper
    result -> result

canExplode :: Zipper -> Bool
canExplode (Regular i, _) = False
canExplode (_, bs) = length bs == 4

explode :: Zipper -> Maybe Zipper
explode zipper@(Pair (Regular x) (Regular y), _) = do
  let addRight z = case rightNeighbour z of
        Nothing -> Just z
        Just rn -> up =<< leftNeighbour =<< addRegular y rn
  let addLeft z = case leftNeighbour z of
        Nothing -> Just z
        Just ln -> up =<< rightNeighbour =<< addRegular x ln
  remove =<< addRight =<< addLeft zipper
explode _ = Nothing

explodeNext :: Number -> Maybe Number
explodeNext = fmap getNumber . explode <=< findFirst canExplode . (,[])

getNumber :: Zipper -> Number
getNumber = fst . topmost

addRegular :: Int -> Zipper -> Maybe Zipper
addRegular x (Regular y, bs) = Just (Regular $ x + y, bs)
addRegular _ _ = Nothing

remove :: Zipper -> Maybe Zipper
remove zipper = do
  (Pair x y, bs) <- up zipper
  let zero = Regular 0
  if isLeft zipper then return (Pair zero y, bs) else return (Pair x zero, bs)

canSplit :: Zipper -> Bool
canSplit (Regular i, _) = i > 9
canSplit _ = False

split :: Zipper -> Maybe Zipper
split (Pair _ _, _) = Nothing
split (Regular i, bs) =
  let n = i `div` 2
      m = i - n
   in Just (Pair (Regular n) (Regular m), bs)

splitNext :: Number -> Maybe Number
splitNext = fmap getNumber . split <=< findFirst canSplit . (,[])

reduce :: Number -> Number
reduce n = case explodeNext n of
  Just x -> reduce x
  Nothing -> maybe n reduce (splitNext n)

instance Show Number where
  show (Regular x) = show x
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = magnitude . foldl1 (<>)

solvePart2 numbers = maximum [magnitude $ x <> y | x <- numbers, y <- numbers, x /= y]

magnitude (Regular i) = i
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

input = number `sepBy` newline

number = (Regular <$> integer) <|> (Pair <$> (char '[' *> number <* char ',') <*> (number <* char ']'))