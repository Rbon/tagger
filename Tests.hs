module Tests where

import Test.QuickCheck
import qualified Tagger
import qualified Data.Text as T

splitFileName :: String -> String -> (String, String)
splitFileName delim str
  | length split == 1 = ("", concat split)
  | otherwise         = (split !! 0, split !! 1)
    where split = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)

prop_split_model d s =
  not (null s) ==>
  not (null d) ==>
    splitFileName d s == Tagger.splitFileName d s

runTests = quickCheck prop_split_model
