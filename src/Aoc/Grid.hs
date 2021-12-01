{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Aoc.Grid where

import Control.Applicative (Applicative (liftA2))
import Data.HashMap.Strict as M (HashMap, fromList, map, toList)
import Data.Hashable (Hashable)
import Data.List (groupBy, intercalate, sort, sortBy, sortOn)
import Data.Ord (comparing)
import Data.Vector as V (Vector, cons, fromList, head, ifoldl', imap, length, map, replicate, snoc, toList, (!), (!?))

newtype Grid a = Grid {toVector :: Vector (Vector a)} deriving (Eq, Functor, Foldable, Traversable, Show)

instance (a ~ Int, b ~ Int) => GridIndex (a, b) where
  toTuple = id
  fromTuple = id

class GridIndex idx where
  fromTuple :: (Int, Int) -> idx
  toTuple :: idx -> (Int, Int)

withBorder :: a -> Grid a -> Grid a
withBorder border grid =
  let sy = sizeY grid
      vs = (\v -> V.cons border $ V.snoc v border) <$> toVector grid
      topBorder = V.replicate (sy + 2) border
   in Grid $ V.cons topBorder $ V.snoc vs topBorder

parseGrid :: [[a]] -> Grid a
parseGrid input = Grid . V.fromList $ [V.fromList line | line <- input]

onVector :: (Vector (Vector a) -> Vector (Vector b)) -> Grid a -> Grid b
onVector f (Grid g) = Grid $ f g

imapP :: GridIndex idx => (idx -> a -> b) -> Grid a -> Grid b
imapP f = onVector $ imap (\y -> imap (\x -> f $ fromTuple (x, y)))

ifoldl' :: GridIndex idx => (a -> idx -> b -> a) -> a -> Grid b -> a
ifoldl' f start grid = V.ifoldl' (\a y v -> V.ifoldl' (\a' x b -> f a' (fromTuple (x, y)) b) a v) start (toVector grid)

mapP :: (a -> b) -> Grid a -> Grid b
mapP f = onVector $ V.map (V.map f)

parseGridMap :: (GridIndex idx, Eq idx, Hashable idx) => [[a]] -> HashMap idx a
parseGridMap input = M.fromList [(fromTuple (x, y), c) | (y, l) <- zip [0 ..] input, (x, c) <- zip [0 ..] l]

fromString :: String -> Grid Char
fromString = parseGrid . lines

fromStringWith :: (Char -> a) -> String -> Grid a
fromStringWith f = mapP f . fromString

toList :: GridIndex idx => Grid a -> [(idx, a)]
toList grid = fmap (\i -> (i, grid Aoc.Grid.! i)) [fromTuple (x, y) | x <- [0 .. sizeX grid - 1], y <- [0 .. sizeY grid - 1]]

fromList :: (GridIndex idx) => [(idx, a)] -> Grid a
fromList values = parseGrid $ fmap (fmap snd) $ groupBy (\a b -> snd (fst a) == snd (fst b)) $ sortOn (\((x, y), _) -> (y, x)) $ [(toTuple idx, a) | (idx, a) <- values]

sizeX :: Grid a -> Int
sizeX = V.length . V.head . toVector

sizeY :: Grid a -> Int
sizeY = V.length . toVector

bounds :: GridIndex idx => Grid a -> idx
bounds = fromTuple . liftA2 (,) sizeX sizeY

inBounds :: GridIndex idx => Grid a -> idx -> Bool
inBounds grid idx =
  let (bX, bY) = bounds grid
      (x, y) = toTuple idx
   in (x >= 0 && y >= 0 && x < bX && y < bY)

(!) :: GridIndex idx => Grid a -> idx -> a
grid ! idx =
  let (x, y) = toTuple idx
   in toVector grid V.! y V.! x

(!?) :: GridIndex idx => Grid a -> idx -> Maybe a
grid !? idx =
  let (x, y) = toTuple idx
   in toVector grid V.!? y >>= (V.!? x)

prettyPrint :: (a -> Char) -> Grid a -> String
prettyPrint toChar = unlines . fmap V.toList . V.toList . toVector . fmap toChar