{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Aoc.Vector where

import Aoc.Grid (GridIndex (..))
import Control.Applicative (Applicative (liftA2))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data V2 a = V2 {_x, _y :: a} deriving (Functor, Foldable, Traversable, Eq, Generic, Hashable)

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Semigroup a => Semigroup (V2 a) where
  V2 x y <> V2 dx dy = V2 (x <> dx) (y <> dy)

instance Applicative V2 where
  pure a = V2 a a
  V2 f g <*> V2 x y = V2 (f x) (g y)

instance Monoid a => Monoid (V2 a) where
  mempty = V2 mempty mempty

instance (a ~ Int) => GridIndex (V2 a) where
  toTuple (V2 x y) = (x, - y)
  fromTuple (x, y) = V2 x (- y)

instance Show a => Show (V2 a) where
  show (V2 x y) = show (x, y)

(.*) :: Num a => a -> V2 a -> V2 a
(.*) s (V2 x y) = V2 (s * x) (s * y)

infixl 7 .*

(*.) :: Num a => V2 a -> a -> V2 a
(*.) = flip (.*)

infixr 7 *.

mapX :: (a -> a) -> V2 a -> V2 a
mapX f (V2 x y) = V2 (f x) y

mapY :: (a -> a) -> V2 a -> V2 a
mapY f (V2 x y) = V2 x (f y)

rotate :: Floating a => a -> V2 a -> V2 a
rotate i (V2 x y) =
  let x' = x * cos i - y * sin i
      y' = x * sin i + y * cos i
   in V2 x' y'

rotateDeg :: Floating a => a -> V2 a -> V2 a
rotateDeg i = rotate (deg2rad i)

flipX :: Num a => V2 a -> V2 a
flipX (V2 x y) = V2 (- x) y

flipY :: Num a => V2 a -> V2 a
flipY (V2 x y) = V2 x (- y)

rot90 :: Num a => V2 a -> V2 a
rot90 (V2 x y) = V2 (- y) x

rot180 :: Num a => V2 a -> V2 a
rot180 (V2 x y) = V2 (- x) (- y)

rot270 :: Num a => V2 a -> V2 a
rot270 (V2 x y) = V2 y (- x)

data DiscreteRotation = CW | CCW | MIRROR | ID deriving (Eq)

instance Semigroup DiscreteRotation where
  ID <> r = r
  r <> ID = r
  MIRROR <> MIRROR = ID
  MIRROR <> CW = CCW
  MIRROR <> CCW = CW
  a <> MIRROR = MIRROR <> a
  a <> b = if a == b then MIRROR else ID

instance Monoid DiscreteRotation where
  mempty = ID

fromAngle :: (MonadFail m, Integral a) => a -> m DiscreteRotation
fromAngle c = case c `mod` 360 of
  0 -> return ID
  90 -> return CCW
  180 -> return MIRROR
  270 -> return CW

rotDiscrete :: Num a => DiscreteRotation -> V2 a -> V2 a
rotDiscrete ID = id
rotDiscrete CCW = rot90
rotDiscrete MIRROR = rot180
rotDiscrete CW = rot270

deg2rad :: Floating a => a -> a
deg2rad d = pi * d / 180

len :: Floating a => V2 a -> a
len (V2 x y) = sqrt (x * x + y * y)

len2 :: Num a => V2 a -> a
len2 (V2 x y) = x * x + y * y

taxiNorm :: Num a => V2 a -> a
taxiNorm (V2 x y) = abs x + abs y

euclidNorm :: (Floating a) => V2 a -> a
euclidNorm = len

maxNorm :: (Num a, Ord a) => V2 a -> a
maxNorm (V2 x y) = max (abs x) (abs y)

south :: Num a => V2 a
south = V2 0 (-1)

east :: Num a => V2 a
east = V2 1 0

north :: Num a => V2 a
north = V2 0 1

west :: Num a => V2 a
west = V2 (-1) 0

origin :: Num a => V2 a
origin = 0