module InitState
  (pickStory)
where

import Control.Monad (forM_)
import System.IO (stdout, hFlush)

import Stories.Types (Story(..))

import qualified Stories.DungeonsOfDoom as DungeonsOfDoom
import qualified Stories.Mangos as Mangos
import qualified Stories.CryoSleep as CryoSleep


readInt :: IO Int
readInt = read <$> getLine


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
  maybe pickStory return (lookup input options)
