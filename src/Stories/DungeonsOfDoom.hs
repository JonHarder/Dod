module Stories.DungeonsOfDoom
  (story)
where

import Color
import GameState
import Stories.Types
import Types

import qualified Data.Map.Strict as Map

starterRoom :: Room
starterRoom = Room
  { rShortDescription = "drip\n\n\ndrip\n\n\n\ndrip"
  , rDescription = "drip...drip....drip.....drip"
  , rInventory = Map.empty
  }

gameState :: GameState
gameState = GameState
  { gRoom = starterRoom
  , gYou = Map.empty
  , gTimeLeft = Time 1000
  , gEvents = Map.empty
  }


story :: Story
story = Story ("Welcome...to the\n\n\t\t" ++ (bold . underline . red) "DUNGEONS OF DOOM") gameState
