{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day18 where

import Aoc.Parsers (Parser, integer)
import Control.Applicative (Alternative ((<|>)))
import Control.Applicative.Combinators (between, sepBy)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL),
    makeExprParser,
  )
import Data.Functor (($>))
import Text.Megaparsec.Char (char, string, newline)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (mkPuzzle, Puzzle)

data Expr = Number Int | Plus Expr Expr | Times Expr Expr

part1 :: Puzzle [Expr] Int
part1 =  mkPuzzle (exprs [[plus, times]]) solve

part2 :: Puzzle [Expr] Int
part2 =  mkPuzzle (exprs [[plus], [times]]) solve

solve :: [Expr] -> Int
solve = sum . fmap eval

eval :: Expr -> Int
eval (Number i) = i
eval (Plus i j) = eval i + eval j
eval (Times i j) = eval i * eval j

exprs :: [[Operator  Parser Expr]] -> Parser [Expr]
exprs ops = expr ops `sepBy` newline

expr :: [[Operator Parser Expr]] -> Parser Expr
expr ops =
  let term = (Number <$> integer) <|> between (char '(') (char ')') (expr ops)
   in makeExprParser term ops

plus :: Operator Parser Expr
plus = InfixL $ string " + " $> Plus

times :: Operator Parser Expr
times = InfixL $ string " * " $> Times