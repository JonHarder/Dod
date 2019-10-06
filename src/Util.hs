module Util 
  ( prompt
  , printMaybe
  ) where

import System.IO


prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  input <- getLine
  return input


printMaybe :: Maybe String -> IO ()
printMaybe ma =
  case ma of
    Just a ->
      putStrLn a
    Nothing ->
      return ()
  
