{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Year2021.Day20 where

import qualified Aoc.Grid as G
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, integer)
import Aoc.Puzzle (Puzzle (Puzzle), mkPuzzle, runPuzzle)
import Aoc.Util (counter, fromBinary, fst3, iterateN, median, minMax)
import Control.Applicative (liftA3)
import Control.Arrow ((<<<), (<<^), (^<<))
import Control.Monad.State.Strict (MonadState (get), State, modify, replicateM, runState, (<=<))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isLower, isOctDigit)
import Data.Either (lefts, rights)
import Data.Foldable (foldl', maximumBy)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap, keys, mapWithKey, (!?))
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (group, intercalate, partition, permutations, scanl', sort, transpose)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Search.BFS.Hashable (bfs, fromSeparate, noAccumulator, runSearch, searchAll, simple)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import Data.Text.Internal.Read (IParser (runP))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Language.Haskell.TH (Extension (OverloadedStrings))
import Text.Megaparsec (count, many, parseMaybe, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, newline, punctuationChar, string, symbolChar)

type Enhancement = Vector Bool

data Image = Image
  { _border :: Bool,
    _image :: HashMap (Int, Int) Bool
  }
  deriving (Show)

part1 = mkPuzzle input (solve 2)

part2 = mkPuzzle input (solve 50)

solve n (enhancement, image) = M.size $ M.filter id $ _image $ iterateN n (nextImage enhancement) image

nextImage :: Enhancement -> Image -> Image
nextImage enhancement image =
  let border = nextBorder enhancement image
      image' = mapWithKey (\pos _ -> nextPixel enhancement image pos) $ _image $ insertBorder image
   in Image border image'

insertBorder :: Image -> Image
insertBorder (Image border image) =
  let (xMin, xMax) = minMax $ fst <$> keys image
      (yMin, yMax) = minMax $ fst <$> keys image
   in Image border $ foldr (`M.insert` border) image ([(x, y) | x <- [xMin -1 .. xMax + 1], y <- [yMin -1, yMax + 1]] ++ [(x, y) | x <- [xMin -1, xMax + 1], y <- [yMin -1 .. yMax + 1]])

nextBorder :: Enhancement -> Image -> Bool
nextBorder enhancement (Image border _) = enhancement V.! fromBinary (value <$> replicate 9 border)

nextPixel :: Enhancement -> Image -> (Int, Int) -> Bool
nextPixel enhancement image (x, y) = enhancement V.! fromBinary [value $ getPixel image (x', y') | y' <- [y -1 .. y + 1], x' <- [x -1 .. x + 1]]

getPixel :: Image -> (Int, Int) -> Bool
getPixel (Image border image) pos = fromMaybe border $ image !? pos

value True = '1'
value False = '0'

input = (,) <$> (enhancementP <* count 2 newline) <*> imageP

imageP :: Parser Image
imageP = Image False . M.fromList . G.toList <$> asciiGrid pixel

enhancementP :: Parser Enhancement
enhancementP = V.fromList <$> many pixel

pixel :: Parser Bool
pixel = (char '#' $> True) <|> (char '.' $> False)