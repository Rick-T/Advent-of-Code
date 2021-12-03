module Year2021.Day03 where

import Aoc.Parsers (Parser)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Aoc.Util (countMatches, fromBinary)
import Data.Function (on)
import Data.List (transpose)
import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (digitChar, newline)

type Digit = Char

type Binary = [Digit]

part1 :: Puzzle [Binary] Int
part1 = mkPuzzle inputP solvePart1

part2 :: Puzzle [Binary] Int
part2 = mkPuzzle inputP solvePart2

solvePart1 :: [Binary] -> Int
solvePart1 = product . fmap fromBinary . sequence [mostCommonBits, leastCommonBits]

solvePart2 :: [Binary] -> Int
solvePart2 = product . fmap fromBinary . traverse rating [mostCommonBits, leastCommonBits]

rating :: ([Binary] -> Binary) -> [Binary] -> Binary
rating bitCriteria =
  let sieve prefix [result] = reverse prefix ++ result
      sieve prefix numbers =
        let (criterion : _) = bitCriteria numbers
         in sieve (criterion : prefix) [bits | (bit : bits) <- numbers, bit == criterion]
   in sieve ""

mostCommonBits :: [Binary] -> Binary
mostCommonBits = fmap mostCommonBit . transpose

leastCommonBits :: [Binary] -> Binary
leastCommonBits = fmap flipBit . mostCommonBits

mostCommonBit :: Binary -> Digit
mostCommonBit number
  | 2 * countMatches (== '1') number >= length number = '1'
  | otherwise = '0'

flipBit :: Digit -> Digit
flipBit '0' = '1'
flipBit '1' = '0'

inputP :: Parser [Binary]
inputP = some digitChar `sepBy` newline