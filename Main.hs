module Main where

import qualified Data.Text as T
import qualified ID3.Simple as S
import qualified System.Process as P

main = do
  str <- getContents
  tagger str

-- returns filename and extension
splitFilename :: String -> [String]
splitFilename str = map T.unpack $ T.splitOn (T.pack ".") (T.pack str)

tagger str = do
  let fileInfo = splitFilename str
  let fileName = fileInfo !! 0
  let fileExt = fileInfo !! 1
  P.system $ "id3v2 -t " ++ fileName ++ " " ++ str
