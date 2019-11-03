module Stories.CryoSleep where

import GameState
import Color
import Data.Map.Strict as Map
import Types
import Stories.Types


initState :: GameState
initState =
  let roomCryoPod = Room
        { rShortDescription = "You see the inside of a CryoPod 3001 that you are lieing down in. You think you might want to take a deeper " ++ Color.green "look" ++ "."
        , rDescription = "In the tight space around you, you see lots of frost. You're lieing on a metal bed, with a cracked glass window in front of you. The window is covered in frost, so you can't see anything beyond it. You don't remember how you got here, and trying seems to hurt your head."
        , rInventory = Map.fromList [(tLabel thingButton, thingButton)]
        }
      thingButton = Thing
        { tDescription = "A large glowing button that says \"Open\", and is covered with a thin layer of frost"
        , tInteraction = TravelRoom "Pushing the button causes the glass to slide off the side, opening your Pod. You sit up, and feel aweful. Something must have gone wrong with your Pod." roomCryoStorage
        , tLabel = Label "button"
        , tRoomDescription = Just $ "To your right is a " ++ Color.blue "button" ++ ". If you " ++ Color.green "interact" ++ " with it the CryoPod should open."
        , tCombinations = Map.empty
        }
      roomCryoStorage = Room
        { rShortDescription = "You are in a small room with the " ++ Color.blue "CryoPod" ++ " that you woke up from."
         , rDescription = "TODO."
         , rInventory = Map.fromList [(tLabel thingCryoPod, thingCryoPod)]
         }
      thingCryoPod = Thing
        { tDescription = "The CryoPod 3001 that you woke up from."
        , tInteraction = Inspect "The CryoPod seems familiar, but there doesn't seem to be anything left to discover here."
        , tLabel = Label "CryoPod"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
--      door = Thing
--        { tDescription = "A large, ornate, wooden door"
--        , tInteraction = TravelRoom "you walk through the door" room2
--        , tLabel = Label "door"
--        , tRoomDescription = Just $ "To the north, there is a large, ornate, wooden " ++ Color.blue "door"
--        , tCombinations = Map.empty
--        }
--      openBox = Thing
--        { tDescription = "Theres a thimble in there"
--        , tInteraction = Inspect "its just a box"
--        , tLabel = Label "box"
--        , tRoomDescription = Just $ "You see an open " ++ Color.blue "box" ++ " in the room"
--        , tCombinations = Map.empty
--        }
--      thimble = Thing
--        { tDescription = "its a thimble, in the box"
--        , tInteraction = Grab "you grab the thimble"
--        , tLabel = Label "thimble"
--        , tRoomDescription = Nothing
--        , tCombinations = Map.fromList [(Label "door", ActOnThing2 (Inspect "You throw your thimble at the door, and watch it bounce off and roll in small circles on the ground. You pick it back up, feeling a little bit silly..."))]
--        }
--      box = Thing
--        { tDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
--        , tInteraction = ReplaceSelfWithThings "You open the box." [openBox, thimble]
--        , tLabel = Label "box"
--        , tRoomDescription = Just $ "There is a " ++ Color.blue "box" ++ " in the corner of the room"
--        , tCombinations = Map.empty
--        }
  in GameState { gRoom = roomCryoPod
               , gYou = Map.empty
            , gTimeLeft = Time 10
            }

story :: Story
story = Story "This is a test story" initState