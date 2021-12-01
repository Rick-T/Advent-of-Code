module Aoc.Parsers where

import Control.Monad (liftM2)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, option, parseMaybe, some, (<|>), anySingleBut, many, sepBy, anySingle, manyTill, MonadParsec (try, notFollowedBy, lookAhead, eof), noneOf, sepEndBy)
import Text.Megaparsec.Char (char, digitChar, newline, symbolChar)
import Aoc.Grid (Grid (Grid), parseGrid)
import Data.Text (Text)

type Parser = Parsec Void Text

integer :: (Integral i, Read i) => Parser i
integer = liftM2 (*) signed positiveInt

signed :: Integral i => Parser i
signed = option 1 $ (char '-' $> (-1)) <|> (char '+' $> 1)

positiveInt :: (Integral i, Read i) => Parser i
positiveInt = read <$> some digitChar

asciiGrid :: Parser a -> Parser (Grid a)
asciiGrid tile = parseGrid <$> some tile `sepBy` newline

inlineAsciiGrid :: Parser a -> Parser (Grid a)
inlineAsciiGrid tile = parseGrid <$> some tile `sepEndBy` newline