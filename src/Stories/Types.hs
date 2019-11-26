{-# LANGUAGE OverloadedStrings #-}
module Stories.Types (Story(..), StoryParseResult(..), loadStory, beginStory) where


import GameState (GameState, gRoom)

import System.Console.Haskeline
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))


data Story = Story { title :: String, welcome :: String, game :: GameState }
  deriving (Show)

instance FromJSON Story where
  parseJSON (Y.Object v) =
    Story <$>
    v .: "title" <*>
    v .: "welcome" <*>
    v .: "game"
  parseJSON _ =
    fail "Object expected when parsing Story"


introduction :: Story -> String
introduction story =
  unlines [welcome story, "", show . gRoom . game $ story]


beginStory :: Story -> (GameState -> InputT IO ()) -> InputT IO ()
beginStory story loop = do
  outputStrLn $ introduction story
  loop $ game story

data StoryParseResult
  = Parsed Story
  | FailedToParseStory String


loadStory :: FilePath -> IO StoryParseResult
loadStory path = do
  result <- Y.decodeFileEither path
  case result of
    Left err -> return $ FailedToParseStory $ show err
    Right story -> return $ Parsed story
