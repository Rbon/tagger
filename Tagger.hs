{-# LANGUAGE OverloadedStrings #-}

module Tagger where

import System.Exit
import qualified Data.Text as T
import qualified ID3 as I
import qualified ID3.Simple as S
import qualified System.Process as P

data FileInfo = FileInfo { fileName :: String
                         , trackNum :: String
                         , title :: String
                         } deriving (Show)

tagger = tagWriter . fileInfo

fileInfo :: String -> FileInfo
fileInfo x = FileInfo { fileName = x, trackNum = y, title = z }
  where (y, rest) = splitFileName " - " x
        (z, _) = splitFileName "." rest

tagInfo :: String -> FileInfo
tagInfo x = FileInfo { fileName = x, trackNum = y, title = z }
  where (y, rest) = splitFileName " - " x
        (z, _) = splitFileName "." rest

tagWriter info = do
  putStrLn $ "writing tags for: " ++ (fileName info)
  S.writeTag (fileName info) (tagMaker info)

tagMaker :: FileInfo -> I.ID3Tag
tagMaker info = S.setTrack (trackNum info) $ S.setTitle (title info) I.emptyID3Tag

usage = "Usage: tagger [-h] [file ..]"
exit  = exitWith ExitSuccess

splitFileName :: String -> String -> (String, String)
splitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (split !! 0, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)
