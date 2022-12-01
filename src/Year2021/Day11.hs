module Year2021.Day11 where

import Aoc.Grid (Grid (Grid), fromList, imapP, inBounds, parseGrid, toList, (!))
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (median)
import Aoc.Vector (V2 (V2), east, north, northEast, northWest, south, southEast, southWest, west)
import Control.Monad.State.Strict (MonadState (get), State, modify, replicateM, runState)
import Data.Char (digitToInt, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Internal.Read (IParser (runP))
import Text.Megaparsec (many, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)

part1 :: Puzzle (Grid (Maybe Int)) Int
part1 = mkPuzzle input solvePart1

part2 :: Puzzle (Grid (Maybe Int)) Int
part2 = mkPuzzle input solvePart2

solvePart1 :: Grid (Maybe Int) -> Int
solvePart1 = sum . fst . runFor 100

solvePart2 :: Grid (Maybe Int) -> Int
solvePart2 = (+ 1) . length . takeWhile (not . all (== Just 0)) . fst . runFor' 10000

runFor :: Int -> Grid (Maybe Int) -> ([Int], Grid (Maybe Int))
runFor i = runState (replicateM i step)

runFor' :: Int -> Grid (Maybe Int) -> ([Grid (Maybe Int)], Grid (Maybe Int))
runFor' i = runState (replicateM i step')

step' :: State (Grid (Maybe Int)) (Grid (Maybe Int))
step' = step *> get

step :: State (Grid (Maybe Int)) Int
step = do
  modify $ fmap (fmap (+ 1))
  i <- flashTillComplete
  modify $ fmap (\val -> if isNothing val then Just 0 else val)
  return i

flashTillComplete :: State (Grid (Maybe Int)) Int
flashTillComplete = do
  flashes <- subStep
  if flashes == 0 then return 0 else (+ flashes) <$> flashTillComplete

subStep :: State (Grid (Maybe Int)) Int
subStep = do
  grid <- get
  let flashers = canFlash grid
  modify $ flash flashers
  return $ length flashers

flash :: [V2 Int] -> Grid (Maybe Int) -> Grid (Maybe Int)
flash flashers grid = imapP (\pos val -> if pos `elem` flashers then Nothing else (+ (length $ filter (\flasher -> pos `elem` neighbours grid flasher) flashers)) <$> val) grid

canFlash :: Grid (Maybe Int) -> [V2 Int]
canFlash grid = [position | (position, Just energy) <- toList grid, energy >= 10]

neighbours :: Grid (Maybe Int) -> V2 Int -> [V2 Int]
neighbours grid = filter (inBounds grid) . traverse (+) [north, south, east, west, northEast, northWest, southEast, southWest]

input :: Parser (Grid (Maybe Int))
input = fmap (Just . digitToInt) <$> asciiGrid digitChar