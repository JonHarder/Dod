module Util 
  ( prompt
  , printMaybe
  , printLines
  , maybeHead
  , firstJust
  , (|>)
  ) where

import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStr, runInputT)
import Data.Foldable (forM_)
import Data.Maybe (mapMaybe)


printMaybe :: Maybe String -> IO ()
printMaybe ma =
  forM_ ma putStrLn


prompt :: String -> InputT IO String
prompt msg = do
  outputStr msg
  i <- getInputLine ""
  case i of
    Nothing ->
      return ""
    Just input ->
      return input

printLines :: [String] -> IO ()
printLines = putStrLn . unlines


firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f =
  maybeHead . mapMaybe f
  

maybeHead :: [a] -> Maybe a
maybeHead s =
  if not $ null s
  then
    Just $ head s
  else
    Nothing


(|>) :: a -> (a -> b) -> b
a |> f = f a
