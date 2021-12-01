{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day16 where

import Aoc.Parsers (Parser, integer)
import Control.Applicative (Alternative (some), (<|>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import Text.Megaparsec (MonadParsec (try), endBy, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, letterChar, newline, spaceChar, string)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (mkPuzzle, Puzzle)

type Rule = (String, Int -> Bool)

type Rules = HashMap String (Int -> Bool)

type Ticket = HashMap Int Int

type Options = HashMap String (HashSet Int)

data Input = Input Rules Ticket [Ticket]

part1 :: Puzzle Input Int
part1 = mkPuzzle inputP solvePart1

part2 :: Puzzle Input Int
part2 = mkPuzzle inputP solvePart2

solvePart1 :: Input -> Int
solvePart1 (Input rules _ otherTickets) = sum $ concatMap (invalidValues rules) otherTickets

solvePart2 :: Input -> Int
solvePart2 (Input rules myTicket otherTickets) = product $ departures myTicket $ reduceOptions $ allOptions rules otherTickets

departures :: Ticket -> HashMap String Int -> [Int]
departures ticket lookup = M.elems $ M.filterWithKey (\key _ -> "departure" `isPrefixOf` key) $ M.compose ticket lookup

reduceOptions :: Options -> HashMap String Int
reduceOptions m = go m mempty
  where
    go uncertain final =
      let isFinalized s = S.size s == 1
          finalized = M.map (head . S.toList) $ M.filter isFinalized uncertain
          dropFinalized s = foldr S.delete s (M.elems finalized)
          newFinal = M.union final finalized
          newUncertain = dropFinalized <$> M.difference uncertain finalized
       in if M.null uncertain then final else go newUncertain newFinal

allOptions :: Rules -> [Ticket] -> Options
allOptions rules tickets =
  let validTickets = filter (isValid rules) tickets
      (map : maps) = fmap (optionsForTicket rules) validTickets
   in foldr (M.unionWith S.intersection) map maps

optionsForTicket :: Rules -> Ticket -> Options
optionsForTicket rules tickets = M.fromList [(name, fieldOptions rule tickets) | (name, rule) <- M.toList rules]

fieldOptions :: (Int -> Bool) -> Ticket -> HashSet Int
fieldOptions rule fields = S.fromList [index | (index, value) <- M.toList fields, rule value]

isValid :: Rules -> Ticket -> Bool
isValid rules = null . invalidValues rules

invalidValues :: Rules -> Ticket -> [Int]
invalidValues rules = filter (\val -> not $ any (\rule -> rule val) rules) . M.elems

inputP :: Parser Input
inputP = do
  rules <- try ruleP `sepEndBy` newline
  _ <- newline *> string "your ticket:" *> newline
  myTicket <- ticketP
  _ <- newline *> newline *> string "nearby tickets:" *> newline
  otherTickets <- ticketP `sepBy` newline
  return $ Input (M.fromList rules) myTicket otherTickets

ruleP :: Parser Rule
ruleP = do
  name <- some (letterChar <|> char ' ')
  _ <- string ": "
  (x1, y1) <- rangeP
  _ <- string " or "
  (x2, y2) <- rangeP
  return (name, \n -> n >= x1 && n <= y1 || n >= x2 && n <= y2)

ticketP :: Parser Ticket
ticketP = M.fromList . zip [0 ..] <$> integer `sepBy` char ','

rangeP :: Parser (Int, Int)
rangeP = (,) <$> integer <*> (char '-' *> integer)