module Aoc.Regex where

import Data.Semiring (Semiring (one, plus, times, zero))

data Regex a = Null | Empty | Unit a | Branch (Regex a) (Regex a) | Chain (Regex a) (Regex a)

instance Semiring (Regex a) where
  zero = Null
  one = Empty
  Null `plus` a = a
  a `plus` Null = a
  Empty `plus` Empty = Empty
  a `plus` b = Branch a b
  Null `times` _ = Null
  _ `times` Null = Null
  a `times` Empty = a
  Empty `times` a = a
  a `times` b = Chain a b

evalRegex :: Semiring b => (a -> b) -> Regex a -> b
evalRegex _ Null = zero
evalRegex _ Empty = one
evalRegex f (Unit a) = f a
evalRegex f (Branch a b) = evalRegex f a `plus` evalRegex f b
evalRegex f (Chain a b) = evalRegex f a `times` evalRegex f b

newtype Printer c = Printer {runPrinter :: [[c]]}

instance Semiring (Printer c) where
  zero = Printer []
  one = Printer [[]]
  (Printer a) `plus` (Printer b) = Printer $ a ++ b
  (Printer a) `times` (Printer b) = Printer [x ++ y | x <- a, y <- b]

newtype Matcher c = Matcher {runMatcher :: [c] -> [[c]]}

instance Semiring (Matcher c) where
  zero = Matcher $ const []
  one = Matcher $ const [[]]
  (Matcher a) `plus` (Matcher b) = Matcher $ \s -> a s ++ b s
  (Matcher a) `times` (Matcher b) = Matcher $ \s ->
    let matches = a s
     in concat [b m | m <- matches]

matches :: Eq c => Regex c -> [c] -> Bool
matches regex = elem [] . runMatcher (evalRegex (Matcher . matchNext) regex)

matchNext :: Eq c => c -> [c] -> [[c]]
matchNext c [] = []
matchNext c (next : rest) = [rest | next == c]

strings :: Regex c -> [[c]]
strings = runPrinter . evalRegex (Printer . return . return)