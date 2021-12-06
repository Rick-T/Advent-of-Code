{-# LANGUAGE OverloadedStrings #-}

module Year2020.Day20 where

import Aoc.Grid
  ( Grid,
    GridIndex (fromTuple),
    bounds,
    fromList,
    imapP,
    parseGrid,
    sizeX,
    sizeY,
    toList,
    (!),
  )
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser, asciiGrid, inlineAsciiGrid, integer)
import Aoc.Puzzle (Puzzle, mkPuzzle)
import Aoc.Util (countMatches, minMax)
import Aoc.Vector (V2 (..), east, north, origin, south, west, (.*))
import Control.Applicative.Combinators (sepBy, sepEndBy, some, (<|>))
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (char, newline, string)

data Tile = Black | White deriving (Eq, Show, Ord)

type Position = V2 Int

type Image = Grid Tile

type Images = [(Int, Image)]

part1 :: Puzzle Images Int
part1 = mkPuzzle imagesP solvePart1

part2 :: Puzzle Images Int
part2 = mkPuzzle imagesP solvePart2

solvePart1 :: Images -> Int
solvePart1 images =
  let solvedPuzzle = puzzle images
      keys = M.keys solvedPuzzle
      xs = _x <$> keys
      ys = _y <$> keys
      (xMin, xMax) = minMax xs
      (yMin, yMax) = minMax ys
   in product [fst $ solvedPuzzle M.! p | p <- uncurry V2 <$> [(x, y) | x <- [xMin, xMax], y <- [yMin, yMax]]]

solvePart2 :: Images -> Int
solvePart2 images =
  let solvedPuzzle = puzzle images
      combined = combine $ dropBorder . snd <$> solvedPuzzle
      baseRoughness = countMatches (== Black) combined
      monsters = maximum [length $ searchMonsters image | image <- orientations combined]
      monsterSize = countMatches (== Black) monster
   in baseRoughness - monsters * monsterSize

searchMonsters :: Image -> [Position]
searchMonsters image =
  let n = bounds image
      vM = bounds monster
      go v l
        | _x (v + vM) < _x n = if hasMonster image v then go (v + east) (v : l) else go (v + east) l
        | _y (v + vM) > _y n = go (v {_x = 0} + south) l
        | otherwise = l
   in go 0 []

hasMonster :: Image -> Position -> Bool
hasMonster image v = and [monsterPiece == White || image ! (v + vM) == Black | (vM, monsterPiece) <- toList monster]

monster :: Image
monster =
  parseGrid $
    fromJust $
      parseMaybe
        (some tileP `sepBy` newline)
        "                  # \n\
        \#    ##    ##    ###\n\
        \ #  #  #  #  #  #   "

combine :: HashMap Position Image -> Image
combine map =
  let tile = map M.! 0
      n = sizeX tile
      vOffset = V2 (minimum $ _x <$> M.keys map) (minimum $ _y <$> M.keys map)
   in fromList [(n .* (vG - vOffset) + vL, tile) | (vG, image) <- M.toList map, (vL, tile) <- toList image]

dropBorder :: Image -> Image
dropBorder image =
  let n = sizeX image
   in fromList [((x, y), image ! (x + 1, y + 1)) | x <- [0 .. n - 3], y <- [0 .. n - 3]]

match :: Position -> (Int, Image) -> (Int, Image) -> Bool
match dir (_, first) (_, second) = border dir first == border (- dir) second

puzzle :: [(Int, Image)] -> HashMap Position (Int, Image)
puzzle [] = mempty
puzzle (image : images) =
  let allOrientations = [(id, orient) | (id, image) <- images, orient <- orientations image]
      buildRow dir p m others =
        case filter (match dir $ m M.! p) others of
          [next] -> buildRow dir (p + dir) (M.insert (p + dir) next m) (dropImage next others)
          [] -> if dir == east then buildRow west p {_x = 0} m others else (m, others)
      buildCol dir p m others =
        let (m', other') = buildRow east p m others
         in case filter (match dir $ m' M.! p) others of
              [next] ->
                buildCol dir (p + dir) (M.insert (p + dir) next m') (dropImage next other')
              [] -> if dir == south then buildCol north p {_y = 0} m' other' else m'
   in buildCol south origin (M.singleton origin image) (dropImage image allOrientations)

dropImage :: (Int, Image) -> [(Int, Image)] -> [(Int, Image)]
dropImage (idx, _) = filter $ (/= idx) . fst

orientations :: Image -> [Image]
orientations image = [rot $ mir image | rot <- take 4 $ iterate (. rotate) id, mir <- [mirror, id]]

mirror :: Image -> Image
mirror = changeIndex (\_ (x, y) -> (y, x))

rotate :: Image -> Image
rotate = changeIndex (\n (x, y) -> (- y + n - 1, x))

changeIndex :: (Int -> (Int, Int) -> (Int, Int)) -> Image -> Image
changeIndex f image = imapP (\p _ -> image ! f (sizeX image) p) image

border :: Position -> Image -> [Tile]
border direction image =
  let x = sizeX image
      y = sizeY image
   in [image ! idx | idx <- borderBounds direction x y]

borderBounds :: Position -> Int -> Int -> [Position]
borderBounds v x y
  | v == south = [fromTuple (i, y - 1) | i <- [0 .. x - 1]]
  | v == north = [fromTuple (i, 0) | i <- [0 .. x - 1]]
  | v == west = [fromTuple (0, i) | i <- [0 .. y - 1]]
  | v == east = [fromTuple (x - 1, i) | i <- [0 .. y - 1]]

imagesP :: Parser Images
imagesP = imageP `sepBy` newline

imageP :: Parser (Int, Image)
imageP = do
  _ <- string "Tile "
  i <- integer
  _ <- char ':'
  _ <- newline
  grid <- inlineAsciiGrid tileP
  return (i, grid)

tileP :: Parser Tile
tileP = ((char '.' <|> char ' ') $> White) <|> (char '#' $> Black)