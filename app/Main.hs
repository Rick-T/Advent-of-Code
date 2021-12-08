{-# LANGUAGE TemplateHaskell #-}

module Main where

import Aoc.Puzzle (Puzzle, runPuzzle)
import Main.Template (mkRun)
import System.Environment (getArgs)
import System.TimeIt (timeItNamed)
import Text.Printf (printf)
import qualified Year2020.Day01
import qualified Year2020.Day02
import qualified Year2020.Day03
import qualified Year2020.Day04
import qualified Year2020.Day05
import qualified Year2020.Day06
import qualified Year2020.Day07
import qualified Year2020.Day08
import qualified Year2020.Day09
import qualified Year2020.Day10
import qualified Year2020.Day11
import qualified Year2020.Day12
import qualified Year2020.Day13
import qualified Year2020.Day14
import qualified Year2020.Day15
import qualified Year2020.Day16
import qualified Year2020.Day17
import qualified Year2020.Day18
import qualified Year2020.Day19
import qualified Year2020.Day20
import qualified Year2020.Day21
import qualified Year2020.Day22
import qualified Year2020.Day23
import qualified Year2020.Day24
import qualified Year2020.Day25
import qualified Year2021.Day01
import qualified Year2021.Day02
import qualified Year2021.Day03
import qualified Year2021.Day04
import qualified Year2021.Day05
import qualified Year2021.Day06
import qualified Year2021.Day07
import qualified Year2021.Day08

$( mkRun
     [ (2020, [1 .. 25]),
       (2021, [1 .. 8])
     ]
 )

main :: IO ()
main = do
  args <- getArgs
  let [year, day] = read <$> args
  timeItNamed "Total Time: " $ run year day

runPuzzlePart :: (Show b) => Int -> Int -> Int -> Puzzle a b -> IO ()
runPuzzlePart year day part puzzle = timeItNamed (printf "Puzzle %04d/%02d/%d" year day part) $ print =<< runPuzzle year day puzzle
