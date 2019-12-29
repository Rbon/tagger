module Main where

import System.Environment
import Tagger

main = do
  args <- getArgs
  mapM tagger args
