module Util 
  ( prompt
  , printMaybe
  , printLines
  , maybeHead
  , (|>)
  ) where

import System.IO


prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  getLine


printMaybe :: Maybe String -> IO ()
printMaybe ma =
  case ma of
    Just a ->
      putStrLn a
    Nothing ->
      return ()


printLines :: [String] -> IO ()
printLines = putStrLn . unlines
  

maybeHead :: [a] -> Maybe a
maybeHead s =
  if length s > 0
  then
    Just $ s !! 0
  else
    Nothing


(|>) :: a -> (a -> b) -> b
a |> f = f a
