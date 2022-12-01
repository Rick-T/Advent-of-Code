{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day22 where

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
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.List (foldl1', group, intercalate, partition, permutations, scanl', sort, transpose)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Search.BFS.Hashable (bfs, fromSeparate, fromSeparateM, listAccumulator, noAccumulator, runSearch, runSearchT, searchAll, searchAllM, simple, simpleM)
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

data Cube = Cube
  { _x, _y, _z :: (Int, Int)
  }
  deriving (Show, Eq, Ord)

data Instruction = Instruction
  { _on :: Bool,
    _cube :: Cube
  }
  deriving (Show)

data Coordinate = Single Int | Between Int Int deriving (Generic, Hashable)

instance Eq Coordinate where
  Single a == Single b = a == b
  Between a b == Between c d = a == c && b == d
  _ == _ = False

instance Show Coordinate where
  show (Single a) = show a
  show (Between a b) = printf "(%d,%d)" a b

type Grid = Set Coordinate

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = solvePart2 . onlySmall

onlySmall = filter (\(Instruction _ (Cube (xMin, xMax) (yMin, yMax) (zMin, zMax))) -> xMin >= -50 && xMax <= 50 && yMin >= -50 && yMax <= 50 && zMin >= -50 && zMax <= 50)

solvePart2 instructions =
  let coords = coordinates instructions
      result = foldl' (executeInstruction coords) HS.empty instructions
   in sum $ volume <$> HS.toList result

volume :: (Coordinate, Coordinate, Coordinate) -> Int
volume (x, y, z) = cVolume x * cVolume y * cVolume z

cVolume (Single _) = 1
cVolume (Between a b) = b - a + 1

executeInstruction :: (Set Int, Set Int, Set Int) -> HashSet (Coordinate, Coordinate, Coordinate) -> Instruction -> HashSet (Coordinate, Coordinate, Coordinate)
executeInstruction (xCoords, yCoords, zCoords) activeCubes (Instruction on (Cube x y z)) =
  let action = if on then HS.insert else HS.delete
      allCoordsFor (pMin, pMax) pCoords = makeCoordinates $ S.takeWhileAntitone (\p -> p <= pMax) $ S.dropWhileAntitone (\p -> p < pMin) pCoords
      xs = allCoordsFor x xCoords
      ys = allCoordsFor y yCoords
      zs = allCoordsFor z zCoords
   in foldr action activeCubes [(x, y, z) | x <- xs, y <- ys, z <- zs]

makeCoordinates :: Set Int -> [Coordinate]
makeCoordinates = go . S.toAscList
  where
    go [] = []
    go [a] = [Single a]
    go (a : b : rest) = Single a : Between (a + 1) (b -1) : go (b : rest)

example = fromJust . parseMaybe input . pack <$> readFile "example.txt"

coordinates instr =
  let getAll f = S.fromList $ concatMap (\(a, b) -> [a, b]) $ fmap (f . _cube) instr
   in (getAll _x, getAll _y, getAll _z)

input :: Parser [Instruction]
input = instructionP `sepBy` newline

instructionP :: Parser Instruction
instructionP = Instruction <$> (onOffP <* char ' ') <*> cubeP

onOffP :: Parser Bool
onOffP = (string "on" $> True) <|> string "off" $> False

cubeP :: Parser Cube
cubeP = Cube <$> (coordinate 'x' <* char ',') <*> (coordinate 'y' <* char ',') <*> coordinate 'z'

coordinate :: Char -> Parser (Int, Int)
coordinate c = (,) <$> (char c *> char '=' *> integer) <*> (string ".." *> integer)

test = parsePuzzleInput 2021 22 input

run = runPuzzle 2021 22

render :: [Instruction] -> IO ()
render = writeFile "test.scad" . foldl' addInstruction ""

addInstruction :: String -> Instruction -> String
addInstruction current (Instruction True cube) = osUnion current (osCube cube)
addInstruction current (Instruction False cube) = osDifference current (osCube cube)

isSmall :: Instruction -> Bool
isSmall (Instruction _ (Cube (xMin, xMax) (yMin, yMax) (zMin, zMax))) = xMin >= -50 && xMax <= 50 && yMin >= -50 && yMax <= 50 && zMin >= -50 && zMax <= 50

osCube :: Cube -> String
osCube (Cube (xMin, xMax) (yMin, yMax) (zMin, zMax)) = printf "translate([%d,%d,%d]){cube([%d,%d,%d]);}" xMin yMin zMin (xMax - xMin + 1) (yMax - yMin + 1) (zMax - zMin + 1)

osUnion :: String -> String -> String
osUnion = printf "union(){%s;%s}"

osDifference :: String -> String -> String
osDifference = printf "difference(){%s;%s}"

osIntersection :: String -> String -> String
osIntersection = printf "intersection(){%s;%s}"