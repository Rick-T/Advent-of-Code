{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day08 where

import Aoc.Parsers (Parser)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.Megaparsec (sepBy, sepEndBy, some)
import Text.Megaparsec.Char (char, letterChar, newline, string)

type Number = Set Char

type Display = ([Number], [Number])

part1 :: Puzzle [Display] Int
part1 = mkPuzzle input solvePart1

part2 :: Puzzle [Display] Int
part2 = mkPuzzle input solvePart2

solvePart1 :: [([Number], [Number])] -> Int
solvePart1 = length . filter (\i -> i == 1 || i == 4 || i == 7 || i == 8) . concatMap (uncurry decodeOutput)

solvePart2 :: [([Number], [Number])] -> Int
solvePart2 = sum . fmap (asNumber . uncurry decodeOutput)

asNumber :: [Int] -> Int
asNumber = foldl (\i j -> 10 * i + j) 0

decodeOutput :: [Number] -> [Number] -> [Int]
decodeOutput patterns = fmap (wireMapping patterns M.!)

wireMapping :: [Number] -> Map Number Int
wireMapping numbers =
  let withLength i = filter ((==) i . length) numbers
      containsAll s cs = all (`elem` s) cs
      with cs = filter (`containsAll` cs)
      without cs = filter (not . (`containsAll` cs))
      includedIn cs = filter (containsAll cs)
      [zeroChars] = without fiveChars $ withLength 6
      [oneChars] = withLength 2
      [twoChars] = without threeChars $ without fiveChars $ withLength 5
      [threeChars] = with sevenChars $ withLength 5
      [fourChars] = withLength 4
      [fiveChars] = includedIn sixChars $ withLength 5
      [sixChars] = without sevenChars $ withLength 6
      [sevenChars] = withLength 3
      [eightChars] = withLength 7
      [nineChars] = with threeChars $ withLength 6
   in M.fromList $ zip [zeroChars, oneChars, twoChars, threeChars, fourChars, fiveChars, sixChars, sevenChars, eightChars, nineChars] [0 .. 9]

input :: Parser [Display]
input = display `sepBy` newline

display :: Parser Display
display = (,) <$> (patterns <* string "| ") <*> output

output :: Parser [Number]
output = number `sepBy` char ' '

patterns :: Parser [Number]
patterns = number `sepEndBy` char ' '

number :: Parser Number
number = S.fromList <$> some letterChar