{-# LANGUAGE OverloadedStrings #-}

module Tagger where

import System.Exit
import qualified Data.Text as T
import qualified ID3.Simple as S
import qualified System.Process as P

data FileInfo = FileInfo { fileName :: String
                         , trackNum :: String
                         , title :: String
                         } deriving (Show)

fileInfo :: String -> FileInfo
fileInfo x = FileInfo { fileName = x, trackNum = y, title = z }
  where (y, rest) = splitFileName " - " x
        (z, _) = splitFileName "." rest

tagger = writer . fileInfo

writer x = do
  trackNumWriter (trackNum x) (fileName x)
  titleWriter (title x) (fileName x)

usage = "Usage: tagger [-h] [file ..]"
exit  = exitWith ExitSuccess

splitFileName :: String -> String -> (String, String)
splitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (split !! 0, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

id3v2Tagger :: String -> String -> String -> IO ExitCode
id3v2Tagger frame content fileName =
  P.system $ "id3v2 "++frame++" '"++content++"' "++"'"++fileName++"'"

trackNumWriter :: String -> String -> IO ExitCode
trackNumWriter = id3v2Tagger "-T"

titleWriter :: String -> String -> IO ExitCode
titleWriter = id3v2Tagger "-t"
