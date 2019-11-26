module Stories.Mangos where

import Color
import GameState
import Stories.Types
import Types

import qualified Data.Map.Strict as Map


starterRoom = Room
  { rShortDescription = "Looming in front of you is mount mango, it would be wise to "
                        ++ blue "look" ++ " around to try and a " ++ green "path" ++ " up the mountain."
  , rDescription = "foo"
  , rInventory = Map.fromList []
  }


gameState = GameState
  { gRoom = starterRoom
  , gYou = Map.empty
  , gTimeLeft = Time 999999
  , gEvents = Map.empty
  }

story = Story
  "Mangos"
  (unlines
    [ "You stand at the foothils of Mount Mango. Growing atop it's jagged peaks is the " ++ yellow "MegaMango" ++ ", it's juicyness and flavor"
    , "the stuff of legends. Your family have sent you, Cara the unicorn, to journey forth, ascend the mountain, and retrieve the mythical mango."
    ])
  gameState
