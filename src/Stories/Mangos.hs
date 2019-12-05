module Stories.Mangos where

import Color
import GameState
import Stories.Types
import Types

import qualified Data.Map.Strict as Map


mkThingMapping :: Thing -> (Label, Thing)
mkThingMapping thing = (tLabel thing, thing)


pathRoom :: Room
pathRoom = undefined


pathThing :: (Label, Thing)
pathThing = mkThingMapping $ Thing "A path" (TravelRoom "you travel up the path" pathRoom) Map.empty (Label "path") Nothing


redHerring :: (Label, Thing)
redHerring = mkThingMapping $ Thing "a small, red fish" Describe Map.empty (Label "herring") (Just $ "A red " ++ green "herring" ++ " lies in front of you, distracting you from your true goal.")


starterRoom :: Room
starterRoom = Room
  { rShortDescription = "Looming in front of you is mount mango, it would be wise to "
                        ++ blue "look" ++ " around to try and a " ++ green "path" ++ " up the mountain."

  , rDescription = "You look around you a bit, taking in the majestic rolling slopes, giving way to the jagged peaks of mount mango.  After a bit of searching, you find what seems to be the most well traveld " ++ green "path" ++ "."

  , rInventory = Map.fromList [pathThing, redHerring]
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
