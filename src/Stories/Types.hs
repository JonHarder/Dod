module Stories.Types (Story(..), beginStory) where


import GameState (GameState, gRoom)


data Story = Story { welcome :: String, game :: GameState }


introduction :: Story -> String
introduction story =
  unlines [welcome story, "", show . gRoom . game $ story]


beginStory :: Story -> (GameState -> IO ()) -> IO ()
beginStory story loop = do
  putStrLn $ introduction story
  loop $ game story
