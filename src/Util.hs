module Util 
  ( prompt
  ) where

import System.IO


prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  input <- getLine
  return input
