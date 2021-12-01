{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Aoc.Input where

import Aoc.Parsers (Parser)
import Text.Megaparsec (parse, ParseErrorBundle (ParseErrorBundle), errorBundlePretty, MonadParsec (eof))
import Control.Exception (catch)
import Text.Printf (printf)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath)
import Control.Monad (unless)
import Network.HTTP.Req (req, GET (GET), NoReqBody (NoReqBody), bsResponse, Url, https, (/:), Scheme (Https), HttpResponse (toVanillaResponse), responseBody, runReq, defaultHttpConfig, header)
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Read (decimal)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.Builder as TB (fromString, toLazyText)
import Data.Text.Lazy (toStrict, unpack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.Megaparsec.Char (newline)
import Data.ByteString.Char8 (strip)
import Data.Text (Text)
import qualified Data.Text.IO as T

readPuzzleInput :: Int -> Int -> IO Text
readPuzzleInput year day = do
    appDir <- getAppUserDataDirectory appDirectory
    let inputDir = joinPath [appDir, inputDirectory year]
    _ <- createDirectoryIfMissing True inputDir
    let inputFile = joinPath [inputDir, filename day]
    inputExists <- doesFileExist inputFile
    if inputExists then T.readFile inputFile else do
        let sessionFile = joinPath [appDir, sessionFilename]
        sessionExists <- doesFileExist sessionFile
        unless sessionExists $ fail $ "No session file: " ++ sessionFile
        sessionCookie <- BS.readFile sessionFile
        input <- downloadPuzzleInput sessionCookie year day
        BS.writeFile inputFile input
        return $ decodeUtf8 input

parsePuzzleInput :: Int -> Int -> Parser a -> IO a
parsePuzzleInput year day parser = do
    result <- parse parser (filename day) <$> readPuzzleInput year day
    case result of
        Right success -> return success
        Left failure -> error $ errorBundlePretty failure

filename :: Int -> String 
filename = printf "Day%02d.txt"

appDirectory :: FilePath 
appDirectory = "AdventOfCode"

inputDirectory :: Int -> FilePath
inputDirectory year = joinPath ["input", printf "%04d" year]

sessionFilename :: FilePath 
sessionFilename = "session"

downloadPuzzleInput :: ByteString -> Int -> Int -> IO ByteString
downloadPuzzleInput sessionCookie year day = do
    let sessionHeader = header "Cookie" sessionCookie
    print "Downloading input"
    response <- runReq defaultHttpConfig $ req GET (inputUrl year day) NoReqBody bsResponse sessionHeader
    return $ strip $ responseBody response

inputUrl :: Int -> Int -> Url Https
inputUrl year day = let
    numberToText = toStrict . TB.toLazyText  . TB.fromString . show
    yearText = numberToText year
    dayText = numberToText day
    in
        https "adventofcode.com" /: yearText /: "day" /: dayText /: "input"
