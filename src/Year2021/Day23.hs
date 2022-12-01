{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day23 where

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

data Dude = A | B | C | D deriving (Eq, Ord, Generic, Hashable, Show)

type Space = Maybe Dude

type Hallway = IntMap Space

data Room = Room
  { _spaces :: [Space],
    _houses :: Dude
  }
  deriving (Eq, Ord, Generic, Hashable, Show)

type Rooms = IntMap Room

instance Hashable a => Hashable (IntMap a) where
  hashWithSalt x im = hashWithSalt x $ IM.toList im

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = snd3 . fromJust . go

solvePart2 = solvePart1 . updateRooms

updateRooms :: (Hallway, Rooms) -> (Hallway, Rooms)
updateRooms (hallway, _) =
  ( hallway,
    IM.fromList $ zip [2, 4, 6, 8] [Room [Just D, Just D, Just D, Just B] A, Room [Just D, Just C, Just B, Just A] B, Room [Just C, Just B, Just A, Just A] C, Room [Just B, Just A, Just C, Just C] D]
  )

targetRoom A = 2
targetRoom B = 4
targetRoom C = 6
targetRoom D = 8

go = runSearch (search $ simple (all isFull . snd)) bfs listAccumulator (fromCombined edges)

draw :: (Hallway, Rooms) -> String
draw (hallway, rooms) =
  unlines
    [ f <$> [-1 .. 11]
      | f <-
          [ const '#',
            drawSpace . (hallway IM.!?)
          ]
            ++ [ drawSpace . fmap (gg 0) . (rooms IM.!?),
                 drawSpace . fmap (gg 1) . (rooms IM.!?),
                 drawSpace . fmap (gg 2) . (rooms IM.!?),
                 drawSpace . fmap (gg 3) . (rooms IM.!?)
               ]
            ++ [ const '#'
               ]
    ]

gg :: Int -> Room -> Space
gg i room
  | length (_spaces room) > i = _spaces room !! i
  | otherwise = Nothing

drawSpace :: Maybe Space -> Char
drawSpace Nothing = '#'
drawSpace (Just Nothing) = '.'
drawSpace (Just (Just dude)) = head $ show dude

isFull :: Room -> Bool
isFull (Room [] _) = True
isFull (Room (Just x : rest) houses) = x == houses && isFull (Room rest houses)
isFull _ = False

edges :: (Hallway, Rooms) -> [((Hallway, Rooms), Sum Int)]
edges (hallway, rooms) = pathsToRoom hallway rooms ++ pathsToHallway hallway rooms

pathsToRoom :: Hallway -> Rooms -> [((Hallway, Rooms), Sum Int)]
pathsToRoom hallway rooms =
  [((IM.insert startPos Nothing hallway, IM.insert targetPos newRoom rooms), cost startPos room dude) | (startPos, Just dude) <- IM.toList $ IM.filter isJust hallway, let targetPos = targetRoom dude, let room = rooms IM.! targetPos, allFreeBetween hallway startPos targetPos, Just newRoom <- [moveIn room dude]]

pathsToHallway :: Hallway -> Rooms -> [((Hallway, Rooms), Sum Int)]
pathsToHallway hallway rooms =
  [((IM.insert targetPos (Just dude) hallway, IM.insert startPos newRoom rooms), cost targetPos newRoom dude) | (startPos, room) <- IM.toList rooms, targetPos <- [0, 1, 3, 5, 7, 9, 10], isNothing (hallway IM.! targetPos), allFreeBetween hallway startPos targetPos, Just (newRoom, dude) <- [moveOut room]]

moveOut :: Room -> Maybe (Room, Dude)
moveOut (Room [] _) = Nothing
moveOut (Room (Nothing : rest) houses) = first fillNothing <$> moveOut (Room rest houses)
moveOut (Room (Just x : rest) houses)
  | x == houses && isFull (Room rest houses) = Nothing
  | otherwise = Just (Room (Nothing : rest) houses, x)

fillNothing :: Room -> Room
fillNothing (Room spaces houses) = Room (Nothing : spaces) houses

cost :: IM.Key -> Room -> Dude -> Sum Int
cost x room dude =
  let dudeCost A = 1
      dudeCost B = 10
      dudeCost C = 100
      dudeCost D = 1000
      result = Sum $ dudeCost dude * (abs (x - targetRoom (_houses room)) + countFree room)
   in result

countFree :: Room -> Int
countFree (Room spaces _) = length $ takeWhile isNothing spaces

allFreeBetween :: Hallway -> IM.Key -> Int -> Bool
allFreeBetween hallway x y
  | x > y = allFreeBetween hallway y x
  | otherwise = all (isNothing . (hallway IM.!)) [x + 1 .. y - 1]

moveIn :: Room -> Dude -> Maybe Room
moveIn (Room spaces houses) dude
  | houses /= dude = Nothing
  | otherwise = go spaces
  where
    go [] = Just $ Room [Just dude] dude
    go (Nothing : rest)
      | isFull (Room rest dude) = Just $ Room (Just dude : rest) dude
      | otherwise = fillNothing <$> moveIn (Room rest dude) dude
    go (Just x : _) = Nothing

input :: Parser (Hallway, Rooms)
input =
  do
    let hallway = IM.fromList $ zip [0 .. 10] (repeat Nothing)
    let rooms = IM.fromList $ zip [2, 4, 6, 8] [Room [Just D, Just B] A, Room [Just D, Just A] B, Room [Just C, Just A] C, Room [Just B, Just C] D]
    return (hallway, rooms)

topP :: Parser [Char]
topP = many $ char '#'

hallwayP :: Parser Hallway
hallwayP = toHallway <$> (char '#' *> many (char '.') <* char '#')

toHallway :: [Char] -> Hallway
toHallway spaces = fromList $ zip [1 .. length spaces] (repeat Nothing)

nextRoomP :: Parser (Int, Dude)
nextRoomP = (,) <$> (length <$> many (char '#')) <*> dudeP

dudeP =
  char 'A' $> A
    <|> char 'B' $> B
    <|> char 'C' $> C
    <|> char 'D' $> D