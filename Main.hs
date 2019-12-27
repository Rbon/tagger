{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified ID3.Simple as S
import qualified System.Process as P

main = do
  args <- getArgs
  let files = map tagger args
  let commands = concatMap extractCommand files
  mapM P.system commands

tagger fileName = (titleP fileName . trackNumP fileName) (fileName, [])

extractCommand (_, xs) = xs

usage = "Usage: tagger [-h] [file ..]"
exit  = exitWith ExitSuccess

unText :: (T.Text -> T.Text) -> String -> String
unText f = T.unpack . f . T.pack

tagParser :: String -> String -> String -> (String, [String]) -> (String, [String])
tagParser delim frame fileName (str, commands) = (rest, command:commands)
    where (content, rest) = splitFileName delim str
          command = id3v2Tagger frame content fileName

trackNumP = tagParser " - " "-T"
titleP = tagParser "." "-t"

splitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (split !! 0, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

id3v2Tagger :: String -> String -> String -> String
id3v2Tagger frame content fileName =
  "id3v2 " ++ frame ++ " '" ++ content ++ "' " ++ "'" ++ fileName ++ "'"
