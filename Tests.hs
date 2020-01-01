module Tests where

import Test.QuickCheck
import Tagger
import qualified Data.Text as T
import qualified ID3 as I
import qualified ID3.Simple as S

model_splitFileName :: String -> String -> (String, String)
model_splitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (head split, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

model_modifyTag :: FilePath -> Maybe S.Tag -> S.Tag
model_modifyTag fileName tag =
  S.setTrack (trackNum info) $ S.setTitle (title info) (safeTag tag)
    where safeTag (Just t) = t
          safeTag Nothing  = I.emptyID3Tag
          info = fileInfo fileName

model_fileInfo :: FilePath -> FileInfo
model_fileInfo x = FileInfo { fileName = x, trackNum = y, title = z }
  where (y, rest) = Tagger.splitFileName " - " x
        (z, _) = Tagger.splitFileName "." rest

prop_split_model d s =
  not (null s) ==>
  not (null d) ==>
    splitFileName d s == model_splitFileName d s

prop_fileInfo_model path info =
  not (null path) ==>
  not (null info) ==>
    fileInfo path == model_fileInfo path

runTests = do
  quickCheck (prop_split_model :: String -> String -> Property)
  quickCheck (prop_fileInfo_model :: String -> String -> Property)
