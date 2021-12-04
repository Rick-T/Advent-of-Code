module Year2021.Day04 where

import Aoc.Parsers (Parser, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle)
import Data.List (inits, sortOn, transpose)
import Text.Megaparsec (many, optional, sepBy, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (char, newline)

type Row = [Int]

type Board = [Row]

type Drawing = [Int]

part1 :: Puzzle (Drawing, [Board]) Int
part1 = mkPuzzle input $ uncurry solvePart1

part2 :: Puzzle (Drawing, [Board]) Int
part2 = mkPuzzle input $ uncurry solvePart2

solvePart1 :: Drawing -> [Board] -> Int
solvePart1 = (head .) . scores

solvePart2 :: Drawing -> [Board] -> Int
solvePart2 = (last .) . scores

scores :: Drawing -> [Board] -> [Int]
scores drawing = fmap (uncurry score) . sortOn (length . fst) . fmap ((,) =<< drawsToComplete drawing)

score :: Drawing -> Board -> Int
score drawing board = last drawing * sum (filter (not . (`elem` drawing)) $ concat board)

drawsToComplete :: Drawing -> Board -> Drawing
drawsToComplete drawing board = head $ dropWhile (not . (`isSolved` board)) $ inits drawing

isSolved :: Drawing -> Board -> Bool
isSolved drawing = any (isComplete drawing) . ((++) <*> transpose)

isComplete :: Drawing -> Row -> Bool
isComplete drawing = all (`elem` drawing)

input :: Parser (Drawing, [Board])
input = (,) <$> (drawingP <* newline <* newline) <*> (boardP `sepBy` newline)

boardP :: Parser Board
boardP = rowP `sepEndBy1` newline

rowP :: Parser Row
rowP = some $ many (char ' ') *> integer

drawingP :: Parser Drawing
drawingP = integer `sepBy` char ','