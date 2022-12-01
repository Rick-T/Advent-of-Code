module Year2021.Day12 where

import Aoc.Grid (Grid (Grid), fromList, imapP, inBounds, parseGrid, toList, (!))
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (fst3, median)
import Aoc.Vector (V2 (V2), east, north, northEast, northWest, south, southEast, southWest, west)
import Control.Monad.State.Strict (MonadState (get), State, modify, replicateM, runState)
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List (group, sort)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Search.BFS.Hashable (bfs, fromSeparate, noAccumulator, runSearch, searchAll, simple)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Internal.Read (IParser (runP))
import Text.Megaparsec (many, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)

type Connection = (String, String)

type Cave = HashMap String (Set String)

type Path = [String]

part1 = mkPuzzle input solvePart1

part2 = mkPuzzle input solvePart2

solvePart1 = length . findAllPaths

solvePart2 = fmap (\l -> (head l, length l)) . group . sort . fmap length . findAllPaths'

input = cave

test = parsePuzzleInput 2021 12 input

run = runPuzzle 2021 12

findAllPaths :: Cave -> [Path]
findAllPaths cave = fst3 <$> runSearch (searchAll (simple isFinished)) bfs noAccumulator (fromSeparate (nextPaths cave) (const $ const $ Just $ Sum 1)) ["start"]

findAllPaths' :: Cave -> [Path]
findAllPaths' cave = fst3 <$> runSearch (searchAll (simple isFinished)) bfs noAccumulator (fromSeparate (nextPaths' cave) (const $ const $ Just $ Sum 1)) ["start"]

isFinished :: Path -> Bool
isFinished ("end" : _) = True
isFinished _ = False

nextPaths :: Cave -> Path -> [Path]
nextPaths _ [] = []
nextPaths cave (current : path) = [next : current : path | next <- S.toList $ cave M.! current, not (isSmall next && next `elem` (current : path))]

nextPaths' :: Cave -> Path -> [Path]
nextPaths' _ [] = []
nextPaths' cave (current : path) = [next : current : path | next <- S.toList $ cave M.! current, canVisit next (current : path)]

canVisit "start" _ = False
canVisit "end" path = not $ "end" `elem` path
canVisit next path
  | any ((== 2) . length) $ group $ sort $ filter isSmall path = not (isSmall next && next `elem` path)
  | otherwise = True

isSmall :: String -> Bool
isSmall = all isLower

cave :: Parser Cave
cave = buildCave <$> connection `sepBy` newline

buildCave :: [Connection] -> Cave
buildCave connections = M.fromListWith S.union $ second S.singleton <$> [(a, b) | (x, y) <- connections, (a, b) <- [(x, y), (y, x)]]

connection :: Parser Connection
connection = (,) <$> (many letterChar <* char '-') <*> many letterChar