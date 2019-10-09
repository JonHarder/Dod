module Util 
  ( prompt
  , printMaybe
  , maybeHead
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
  

maybeHead :: [a] -> Maybe a
maybeHead s = if length s > 0
              then
                Just $ s !! 0
              else
                Nothing
