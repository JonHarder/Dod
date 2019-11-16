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
  { rShortDescription = unlines
    ["\"Finally, you're awake.  I was starting to think I hit you a bit too hard when I knocked you out...\""
    , ""
    , "You look around for the source of the words, eventually finding a lumpy suit of leather armor,"
    , "with an equally lumpy, middle aged man uncomfortably squeezed inside."
    , ""
    , "\"I didn't have to stick around until you woke up you know, most guards just chuck people like you into their cell and check in on them a few days later.\""
    , "" , ""
    , "\"...Not that I was worried, mind you.\""
    ]
  , rDescription = "You take in the glory that is your presumed captor. Plastered off-center on his armor is a small piece of parchment with the message\n\n    HELLO, MY NAME IS\n         " ++ blue "steve"
  , rInventory = Map.fromList [(Label "steve", Thing "You prison guard, he looks happy to have someone to talk to." (Inspect "You can't do anything to him with these bars in the way.") Map.empty (Label "steve") Nothing)]
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
