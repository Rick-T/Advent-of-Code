{-# LANGUAGE AllowAmbiguousTypes #-}

module Aoc.Puzzle where
import Aoc.Parsers (Parser)
import Aoc.Input (parsePuzzleInput)

data Puzzle a b = Puzzle {
    _parser :: Parser a,
    _solve :: a -> b
}

mkPuzzle :: Parser a -> (a -> b) -> Puzzle a b
mkPuzzle = Puzzle

runPuzzle :: Int -> Int -> Puzzle a b -> IO b
runPuzzle year day (Puzzle parser solve) = solve <$> parsePuzzleInput year day parser