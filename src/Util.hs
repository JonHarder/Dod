module Util 
  ( prompt
  , printMaybe
  , printLines
  , maybeHead
  , firstJust
  , (|>)
  ) where

import System.Console.Readline (readline, addHistory)
import Data.Foldable (forM_)
import Data.Maybe (mapMaybe)


prompt :: String -> IO String
prompt message = do
  input <- readline message
  case input of
    Just response -> do
      addHistory response
      return response
    Nothing ->
      return ""


printMaybe :: Maybe String -> IO ()
printMaybe ma =
  forM_ ma putStrLn


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
