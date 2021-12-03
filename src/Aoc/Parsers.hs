module Aoc.Parsers where

import Aoc.Grid (Grid (Grid), parseGrid)
import Control.Monad (liftM2)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof, lookAhead, notFollowedBy, try), Parsec, anySingle, anySingleBut, many, manyTill, noneOf, option, parseMaybe, sepBy, sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, newline, symbolChar)

type Parser = Parsec Void Text

integer :: (Integral i, Read i) => Parser i
integer = (*) <$> signed <*> positiveInt

signed :: Integral i => Parser i
signed = option 1 $ (char '-' $> (-1)) <|> (char '+' $> 1)

positiveInt :: (Integral i, Read i) => Parser i
positiveInt = read <$> some digitChar

asciiGrid :: Parser a -> Parser (Grid a)
asciiGrid tile = parseGrid <$> some tile `sepBy` newline

inlineAsciiGrid :: Parser a -> Parser (Grid a)
inlineAsciiGrid tile = parseGrid <$> some tile `sepEndBy` newline