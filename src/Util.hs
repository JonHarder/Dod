module Util 
  ( prompt
  , printMaybe
  , printLines
  , maybeHead
  , (|>)
  ) where

import System.IO
import Data.Foldable (forM_)


prompt :: String -> IO String
prompt message = do
  putStr message
  hFlush stdout
  getLine


printMaybe :: Maybe String -> IO ()
printMaybe ma =
  forM_ ma putStrLn


printLines :: [String] -> IO ()
printLines = putStrLn . unlines
  

maybeHead :: [a] -> Maybe a
maybeHead s =
  if not $ null s
  then
    Just $ head s
  else
    Nothing


(|>) :: a -> (a -> b) -> b
a |> f = f a
