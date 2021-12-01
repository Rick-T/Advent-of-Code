module Main.Template where

import Text.Printf (printf)
import Language.Haskell.TH
    ( mkName,
      Exp(VarE, AppE, DoE, LitE),
      Clause(Clause),
      Q,
      Pat(LitP),
      Dec(FunD),
      Name,
      Body(NormalB),
      Lit(IntegerL), Stmt (NoBindS) )
import Aoc.Util (uncurry3)

mkRun :: [(Integer, [Integer])] -> Q [Dec]
mkRun puzzles = return [ FunD runPuzzleName [mkRunClause year day | (year, days) <- puzzles, day <- days]]

mkRunClause :: Integer -> Integer -> Clause
mkRunClause year day = 
      Clause [
      LitP $ IntegerL year,
      LitP $ IntegerL day
      ]
      (NormalB $ DoE $ NoBindS . runPartExpression year day <$> [1, 2])
      [] 

runPartExpression :: Integer -> Integer -> Integer -> Exp
runPartExpression year day part = let
    func = VarE runPuzzlePartName
    appYear = AppE func $ LitE $ IntegerL year
    appDay = AppE appYear $ LitE $ IntegerL day
    appPart = AppE appDay $ LitE $ IntegerL part
    appPuzzle = AppE appPart $ VarE $ mkName $ printf "Year%04d.Day%02d.part%d" year day part
    in appPuzzle

runPuzzleName :: Name
runPuzzleName = mkName "run"

runPuzzlePartName :: Name
runPuzzlePartName = mkName "runPuzzlePart"