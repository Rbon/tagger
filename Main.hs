module Main where

import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified ID3.Simple as S
import qualified System.Process as P

main = getArgs >>= parseArgs >>= putStrLn

parseArgs :: [String] -> IO String
parseArgs ["-h"] = usage >> exit
parseArgs []     = getContents
parseArgs files  = tagger files

usage = putStrLn "Usage: tagger [-h] [file ..]"
exit  = exitWith ExitSuccess

-- returns filename and extension
splitFilename :: String -> [String]
splitFilename str = map T.unpack $ T.splitOn (T.pack ".") (T.pack str)

tagger :: [[Char]] -> IO a
tagger (str:strs) = do
  let fileInfo = splitFilename str
  let fileName = fileInfo !! 0
  let fileExt = fileInfo !! 1
  P.system $ "id3v2 -t " ++ fileName ++ " " ++ str
  tagger strs
tagger _ = exit

