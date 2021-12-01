module Year2020.Day08 where

import Aoc.Parsers (Parser, integer)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    gets,
    modify,
  )
import Data.IntSet as S (IntSet, insert, member)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V (fromList, length, modify, (!))
import Data.Vector.Generic.Mutable (write)
import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (letterChar, newline, spaceChar)
import Aoc.Input (parsePuzzleInput)
import Aoc.Puzzle (Puzzle, mkPuzzle)

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show)

type Program = Vector Instruction

data Computer = Computer {instr :: Int, acc :: Int, executed :: IntSet}

type ComputerState = State Computer

type ProgramResult = Maybe Int

data StepResult = Continue | Stop ProgramResult

part1 :: Puzzle Program Int
part1 = mkPuzzle program solvePart1

part2 :: Puzzle Program ProgramResult
part2 = mkPuzzle program solvePart2 

solvePart1 :: Program -> Int
solvePart1 p = evaluate $ runProgram p *> gets acc

solvePart2 :: Program -> ProgramResult
solvePart2 p = evaluate $ runPrograms (fixProgram p)

evaluate :: ComputerState a -> a
evaluate = flip evalState (Computer 0 0 mempty)

fixProgram :: Program -> [Program]
fixProgram p = mapMaybe (modifyAt p) [0 .. V.length p - 1]

modifyAt :: Program -> Int -> Maybe Program
modifyAt p i = case p V.! i of
  Nop n -> Just $ V.modify (\v -> write v i (Jmp n)) p
  Acc _ -> Nothing
  Jmp j -> Just $ V.modify (\v -> write v i (Nop j)) p

runProgram :: Program -> ComputerState ProgramResult
runProgram p = do
  result <- stepProgram p
  case result of
    Stop r -> return r
    Continue -> runProgram p

runPrograms :: [Program] -> ComputerState ProgramResult
runPrograms [] = return Nothing
runPrograms (p : ps) = do
  result <- runProgram p
  case result of
    Nothing -> reset *> runPrograms ps
    success -> return success

stepProgram :: Program -> ComputerState StepResult
stepProgram p = do
  c <- get
  case stepResult c p of
    Continue -> Continue <$ executeNext p
    stop -> return stop

executeNext :: Program -> ComputerState ()
executeNext p = do
  i <- gets instr
  updateExecuted i <* case p V.! i of
    Nop _ -> increaseInstr 1
    Acc v -> increaseAcc v *> increaseInstr 1
    Jmp v -> increaseInstr v

stepResult :: Computer -> Program -> StepResult
stepResult (Computer i a e) p
  | i == V.length p = Stop (Just a)
  | i > V.length p || i `S.member` e = Stop Nothing
  | otherwise = Continue

reset :: ComputerState ()
reset = put $ Computer 0 0 mempty

increaseInstr :: Int -> ComputerState ()
increaseInstr i = modify $ \s -> s {instr = instr s + i}

increaseAcc :: Int -> ComputerState ()
increaseAcc a = modify $ \s -> s {acc = acc s + a}

updateExecuted :: Int -> ComputerState ()
updateExecuted i = modify $ \s -> s {executed = S.insert i $ executed s}

program :: Parser Program
program = V.fromList <$> instruction `sepBy` newline

instruction :: Parser Instruction
instruction = opCode <* spaceChar <*> integer

opCode :: Parser (Int -> Instruction)
opCode = do
  code <- some letterChar
  case code of
    "nop" -> return Nop
    "acc" -> return Acc
    "jmp" -> return Jmp
    _ -> fail "Invalid opCode"