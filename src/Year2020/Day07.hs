{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day07 where

import Aoc.Parsers (Parser)
import Data.Functor (($>))
import Data.HashMap.Strict as M (HashMap, adjust, fromList, insert, lookup, member, (!))
import Data.HashSet (HashSet)
import qualified Data.HashSet as S (insert, map, singleton, size, union)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Text.Megaparsec (endBy, optional, sepBy, some, (<|>))
import Text.Megaparsec.Char
  ( char,
    digitChar,
    letterChar,
    newline,
    spaceChar,
    string,
  )
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (Puzzle, mkPuzzle)

data Bag = Bag String String deriving (Show, Eq, Ord, Generic)

instance Hashable Bag

type Rule = (Bag, [Content])

type Content = (Int, Bag)

type Rules = HashMap Bag [Content]

type Parents = HashMap Bag (HashSet Bag)

part1 :: Puzzle Parents Int
part1 = mkPuzzle parents solvePart1

part2 :: Puzzle Rules Int
part2 = mkPuzzle rules solvePart2

solvePart1 :: Parents -> Int
solvePart1 = S.size . (`allParents` shinyGold)

solvePart2 :: Rules -> Int
solvePart2 = (`countNested` shinyGold)
 
shinyGold :: Bag
shinyGold = Bag "shiny" "gold"

allParents :: Parents -> Bag -> HashSet Bag
allParents m target =
  let mParents = M.lookup target m
   in case mParents of
        Nothing -> mempty
        Just parents -> foldr S.union parents $ S.map (allParents m) parents

countNested :: Rules -> Bag -> Int
countNested m target =
  let nested = m ! target
      selfCount = sum $ map fst nested
      nestedCount = sum $ (\(c, n) -> c * countNested m n) <$> nested
   in selfCount + nestedCount

invertRelations :: [Rule] -> Parents
invertRelations = foldr insertInverted mempty

insertInverted :: Rule -> Parents -> Parents
insertInverted (parent, nested) map =
  let children = snd <$> nested
   in foldr (safeInsert parent) map children

safeInsert :: Bag -> Bag -> Parents -> Parents
safeInsert parent child m = if child `member` m then adjust (S.insert parent) child m else M.insert child (S.singleton parent) m

parents :: Parser Parents
parents = invertRelations <$> (rule `sepBy` newline)

rules :: Parser Rules
rules = fromList <$> (rule `sepBy` newline)

rule :: Parser Rule
rule =
  (,)
    <$> (bag <* char ' ' <* bagWord <* string " contain ")
    <*> ((noBags <|> someBags) <* char '.')

noBags :: Parser [Content]
noBags = string "no other bags" $> []

someBags :: Parser [Content]
someBags = bagWithCount `endBy` (char ' ' *> bagWord *> optional (string ", "))

bagWord :: Parser ()
bagWord = string "bag" *> optional (char 's') $> ()

bagWithCount :: Parser Content
bagWithCount =
  (,)
    <$> (read <$> some digitChar)
    <*> (spaceChar *> bag)

bag :: Parser Bag
bag =
  Bag
    <$> some letterChar
    <*> (spaceChar *> some letterChar)
