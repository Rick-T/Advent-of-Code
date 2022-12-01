module Year2021.Day10 where

import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (median)
import Data.Char (isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec (many, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, letterChar, newline, punctuationChar, string, symbolChar)

part1 :: Puzzle [String] Int
part1 = mkPuzzle input solvePart1

part2 :: Puzzle [String] Int
part2 = mkPuzzle input solvePart2

solvePart1 :: [String] -> Int
solvePart1 = sum . fmap syntaxScore . lefts . fmap check

solvePart2 :: [String] -> Int
solvePart2 = median . sort . fmap autocompleteScore . rights . fmap check

check :: String -> Either Char String
check = go []
  where
    go acc [] = Right acc
    go acc (b : bs)
      | isOpening b = go (b : acc) bs
      | otherwise = if matches (head acc) b then go (tail acc) bs else Left b

autocompleteScore :: [Char] -> Int
autocompleteScore = foldl' (\a b -> 5 * a + score b) 0
  where
    score '(' = 1
    score '[' = 2
    score '{' = 3
    score '<' = 4

syntaxScore :: Num p => Char -> p
syntaxScore ')' = 3
syntaxScore ']' = 57
syntaxScore '}' = 1197
syntaxScore '>' = 25137

matches :: Char -> Char -> Bool
matches '[' ']' = True
matches '(' ')' = True
matches '<' '>' = True
matches '{' '}' = True
matches _ _ = False

isOpening :: Char -> Bool
isOpening '[' = True
isOpening '(' = True
isOpening '<' = True
isOpening '{' = True
isOpening _ = False

input :: Parser [String]
input = many (symbolChar <|> punctuationChar) `sepBy` newline