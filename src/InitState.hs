module InitState
  (pickStory)
where

import Control.Monad (forM_)
import System.IO (stdout, hFlush)
import Text.Read (readMaybe)

import Stories.Types (Story(..))

import qualified Stories.DungeonsOfDoom as DungeonsOfDoom
import qualified Stories.Mangos as Mangos
import qualified Stories.CryoSleep as CryoSleep


readInt :: IO (Maybe Int)
readInt = readMaybe <$> getLine


pickStory :: IO Story
pickStory = do
  let stories = [ DungeonsOfDoom.story
                , Mangos.story
                , CryoSleep.story
                ]
      options = zip [1..] stories
  forM_ options $ \(index, story) ->
    putStrLn $ show index ++ ": " ++ title story
  putStr "enter number of story: "
  hFlush stdout
  input <- readInt
  case input of
    Just i ->
      maybe pickStory return (lookup i options)
    Nothing ->
      pickStory
