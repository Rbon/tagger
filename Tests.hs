module Tests where

import Test.QuickCheck
import Tagger
import qualified Data.Text as T
import qualified ID3 as I
import qualified ID3.Simple as S

modelSplitFileName :: String -> String -> (String, String)
modelSplitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (head split, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

modelModifyTag :: FilePath -> Maybe S.Tag -> S.Tag
modelModifyTag fileName tag =
  S.setTrack (trackNum info) $ S.setTitle (title info) (safeTag tag)
    where safeTag (Just t) = t
          safeTag Nothing  = I.emptyID3Tag
          info = fileInfo fileName

modelFileInfo :: FilePath -> FileInfo
modelFileInfo x = FileInfo { fileName = x, trackNum = y, title = z }
  where (y, rest) = Tagger.splitFileName " - " x
        (z, _) = Tagger.splitFileName "." rest

prop_split_model d s =
  not (null s) ==>
  not (null d) ==>
    splitFileName d s == modelSplitFileName d s

prop_fileInfo_model path info =
  not (null path) ==>
  not (null info) ==>
    fileInfo path == modelFileInfo path

runTests = do
  quickCheck (prop_split_model :: String -> String -> Property)
  quickCheck (prop_fileInfo_model :: String -> String -> Property)
