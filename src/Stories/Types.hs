module Stories.Types (Story(..), beginStory) where


import GameState (GameState, gRoom)

import System.Console.Haskeline


data Story = Story { welcome :: String, game :: GameState }


introduction :: Story -> String
introduction story =
  unlines [welcome story, "", show . gRoom . game $ story]


beginStory :: Story -> (GameState -> InputT IO ()) -> InputT IO ()
beginStory story loop = do
  outputStrLn $ introduction story
  loop $ game story
