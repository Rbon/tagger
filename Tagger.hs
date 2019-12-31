{-# LANGUAGE OverloadedStrings #-}

module Tagger where

import System.Exit
import qualified Data.Text as T
import qualified ID3 as I
import qualified ID3.Simple as S

data FileInfo = FileInfo { fileName :: String
                         , trackNum :: String
                         , title :: String
                         } deriving (Show)

tagger :: FilePath -> IO ()
tagger fileName = reader >>= modifier >>= writer
  where reader = S.readTag fileName
        modifier = return . (modifyTag fileName)
        writer = tagWriter fileName

modifyTag :: FilePath -> Maybe S.Tag -> S.Tag
modifyTag fileName tag =
  S.setTrack (trackNum info) $ S.setTitle (title info) (safeTag tag)
    where safeTag (Just t) = t
          safeTag Nothing  = I.emptyID3Tag
          info = fileInfo fileName

fileInfo :: String -> FileInfo
fileInfo x = FileInfo { fileName = x, trackNum = y, title = z }
  where (y, rest) = splitFileName " - " x
        (z, _) = splitFileName "." rest

tagWriter :: FilePath -> S.Tag -> IO ()
tagWriter fileName tag = do
  putStrLn $ "writing tags for: " ++ fileName
  S.writeTag fileName tag

splitFileName :: String -> String -> (String, String)
splitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (split !! 0, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)
