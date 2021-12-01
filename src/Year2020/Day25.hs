module Year2020.Day25 where

import Aoc.Parsers ( Parser, integer )
import Control.Applicative (Applicative(liftA2))
import Text.Megaparsec.Char (newline)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (mkPuzzle, Puzzle)
-- >>> part1
-- 4968512
part1 :: Puzzle (Integer, Integer) Integer
part1 = mkPuzzle inputP solvePart1

-- >>> part2
-- "⭐"
part2 :: Puzzle String String
part2 = mkPuzzle (return "⭐") id

solvePart1 :: (Integer, Integer) -> Integer 
solvePart1 (pkCard, pkDoor) = encryptionKey (loopSize pkCard) pkDoor

inputP :: Parser (Integer, Integer)
inputP = liftA2 (,) (integer <* newline) integer 

encryptionKey :: Int -> Integer -> Integer
encryptionKey loopSize publicKey = apply loopSize publicKey 1

loopSize :: Integer -> Int
loopSize publicKey = length $ takeWhile (/= publicKey) $ iterate (apply 1 7) 1

apply :: Int -> Integer -> Integer -> Integer
apply loops subjectNumber value = (subjectNumber ^ loops * value) `rem` 20201227