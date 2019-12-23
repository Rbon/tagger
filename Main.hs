module Main where

import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified ID3.Simple as S
import qualified System.Process as P

main = getArgs >>= parseArgs

parseArgs files = mapM tagger files

usage = "Usage: tagger [-h] [file ..]"
exit  = exitWith ExitSuccess

parseTrackNum :: String -> [String]
parseTrackNum str = map T.unpack $ T.splitOn (T.pack " - ") (T.pack str)

parseTitle :: String -> [String]
parseTitle str = map T.unpack $ T.splitOn (T.pack ".") (T.pack str)

-- this is poor form
parseTags :: String -> [String]
parseTags fileName = do
  let tokens = parseTrackNum fileName
  let track = tokens !! 0
  let str = tokens !! 1
  let tokens = parseTitle str
  let title = tokens !! 0
  let ext = tokens !! 1
  [fileName, track, title, ext]

id3v2Tagger (fileName:track:title:ext) = do
  P.system $ "id3v2 -T '" ++ track ++ "' " ++ "'" ++ fileName ++ "'"
  P.system $ "id3v2 -t '" ++ title ++ "' " ++ "'" ++ fileName ++ "'"

tagger = id3v2Tagger . parseTags

