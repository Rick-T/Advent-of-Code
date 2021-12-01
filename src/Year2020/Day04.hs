module Year2020.Day04 where

import Aoc.Util (countMatches)
import Data.Set as S (Set, insert, size)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, try),
    Parsec,
    anySingle,
    count,
    sepBy1,
    sepEndBy1,
    some,
    someTill,
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    hexDigitChar,
    lowerChar,
    newline,
    printChar,
    spaceChar,
  )
import Aoc.Input (parsePuzzleInput)
import Aoc.Parsers (Parser)
import Aoc.Puzzle (Puzzle, mkPuzzle)

data Passport = Passport {getFields :: Set PassField, getGarbage :: [Garbage]}

data Unit = CM | IN deriving (Eq, Ord, Show)

data EyeColour = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Eq, Ord, Show)

data PassField = BYR Int | IYR Int | EYR Int | HGT Int Unit | HCL String | ECL EyeColour | PID Int deriving (Eq, Ord, Show)

data Garbage = Garbage {getKey :: String, getValue :: String}

data Entry = Valid PassField | Invalid Garbage | CID

part1 :: Puzzle [Passport] Int
part1 = mkPuzzle  passports solvePart1

part2 :: Puzzle [Passport] Int
part2 = mkPuzzle passports solvePart2

solvePart1 :: [Passport] -> Int
solvePart1 = countMatches enoughFields

solvePart2 :: [Passport] -> Int
solvePart2 = countMatches (\a -> enoughFields a && allValid a)

allValid :: Passport -> Bool
allValid (Passport _ []) = True
allValid _ = False

enoughFields :: Passport -> Bool
enoughFields (Passport f g) = S.size f + length g >= 7

passports :: Parser [Passport]
passports = passport `sepBy1` newline

passport :: Parser Passport
passport =
  fromEntries <$> passField `sepEndBy1` spaceChar

fromEntries :: [Entry] -> Passport
fromEntries = foldr addEntry (Passport mempty [])

addEntry :: Entry -> Passport -> Passport
addEntry (Valid field) p = p {getFields = S.insert field $ getFields p}
addEntry (Invalid garbage) p = p {getGarbage = garbage : getGarbage p}
addEntry CID p = p

passField :: Parser Entry
passField = do
  t <- someTill printChar $ char ':'
  fieldValue t

fieldValue :: String -> Parser Entry
fieldValue t =
  do
    try
      ( case t of
          "byr" -> Valid . BYR <$> numberBetween 1920 2002
          "iyr" -> Valid . IYR <$> numberBetween 2010 2020
          "eyr" -> Valid . EYR <$> numberBetween 2020 2030
          "hgt" -> Valid . uncurry HGT <$> height
          "hcl" -> Valid . HCL <$> hairColour
          "ecl" -> Valid . ECL <$> eyeColour
          "pid" -> Valid . PID <$> passportId
          "cid" -> parseFallback >> return CID
          _ -> fail "Field is garbage"
          <* lookAhead fieldEnd
      )
    <|> Invalid . Garbage t <$> parseFallback

parseFallback :: Parser String
parseFallback = someTill anySingle $ lookAhead fieldEnd

passportId :: Parser Int
passportId = read <$> count 9 digitChar

fieldEnd :: Parser ()
fieldEnd = return () <* spaceChar <|> eof

eyeColour :: Parser EyeColour
eyeColour = do
  code <- count 3 lowerChar
  case code of
    "amb" -> return AMB
    "blu" -> return BLU
    "brn" -> return BRN
    "gry" -> return GRY
    "grn" -> return GRN
    "hzl" -> return HZL
    "oth" -> return OTH
    _ -> fail "Invalid eye colour"

hairColour :: Parser String
hairColour = (:) <$> char '#' <*> count 6 hexDigitChar

height :: Parser (Int, Unit)
height = do
  h <- read <$> some digitChar
  u <- count 2 lowerChar
  case u of
    "cm" -> (,) <$> checkBounds h 150 193 <*> return CM
    "in" -> (,) <$> checkBounds h 59 76 <*> return IN
    _ -> fail "Invalid unit"

numberBetween :: Int -> Int -> Parser Int
numberBetween min max = do
  year <- read <$> some digitChar
  checkBounds year min max

checkBounds :: MonadFail m => Int -> Int -> Int -> m Int
checkBounds num min max = if num < min || num > max then fail "Number not in range" else return num