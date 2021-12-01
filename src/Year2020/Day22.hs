module Year2020.Day22 where

import Aoc.Parsers (Parser, integer)
import Aoc.Queue (Queue)
import qualified Aoc.Queue as Q
import Control.Applicative.Combinators (manyTill, sepEndBy)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Text.Megaparsec.Char (newline, printChar)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (mkPuzzle, Puzzle)

type Deck = Queue Int

data Winner = P1 Deck | P2 Deck

part1 :: Puzzle (Deck, Deck) Int
part1 = mkPuzzle inputP solvePart1

part2 :: Puzzle (Deck, Deck) Int
part2 = mkPuzzle  inputP solvePart2

solvePart1 :: (Deck, Deck) -> Int
solvePart1 = score . getDeck . playRound

solvePart2 :: (Deck, Deck) -> Int
solvePart2 = score . getDeck . playRecursive mempty

score :: Deck -> Int
score deck = sum $ zipWith (*) [1 ..] $ Q.toListR deck

playRecursive :: HashSet (Int, Int) -> (Deck, Deck) -> Winner
playRecursive previous (d1, d2) = case (Q.pop d1, Q.pop d2) of
  (Nothing, _) -> P2 d2
  (_, Nothing) -> P1 d1
  (Just (i1, d1'), Just (i2, d2'))
    | (s1, s2) `S.member` previous -> P1 d1
    | Q.size d1' >= i1 && Q.size d2' >= i2 -> case playRecursive mempty (Q.take i1 d1', Q.take i2 d2') of
      P1 d -> oneWins
      P2 d -> twoWins
    | i1 > i2 -> oneWins
    | otherwise -> twoWins
    where
      s1 = score d1
      s2 = score d2
      previous' = S.insert (s1, s2) previous
      oneWins = playRecursive previous' (Q.push i2 $ Q.push i1 d1', d2')
      twoWins = playRecursive previous' (d1', Q.push i1 $ Q.push i2 d2')

playRound :: (Deck, Deck) -> Winner
playRound (d1, d2) = case (Q.pop d1, Q.pop d2) of
  (Nothing, _) -> P2 d2
  (_, Nothing) -> P1 d1
  (Just (i1, d1'), Just (i2, d2'))
    | i1 > i2 -> playRound (Q.push i2 $ Q.push i1 d1', d2')
    | otherwise -> playRound (d1', Q.push i1 $ Q.push i2 d2')

getDeck :: Winner -> Deck
getDeck (P1 d) = d
getDeck (P2 d) = d

inputP :: Parser (Deck, Deck)
inputP = (,) <$> (deckP <* newline) <*> deckP

deckP :: Parser Deck
deckP = manyTill printChar newline *> (Q.fromList <$> integer `sepEndBy` newline)