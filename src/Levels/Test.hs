module Levels.Test where


import GameState
import Color
import Data.Map.Strict as Map
import Types


initState :: GameState
initState =
  let room2 = Room
        { rShortDescription = "the next room"
        , rDescription = "A completely blank, white room"
        , rInventory = Map.empty
        }
      room1 = Room
        { rShortDescription = "The first room, not much to do here as far as you can tell. Maybe you should " ++ Color.green "look" ++ " around a bit."
        , rDescription = "A small dingey dungeon, with a kiddy pool in the center, and a weird table in the corner."
        , rInventory = i
        }
      boat = Thing
        { tDescription = "There's a boat here, for some reason."
        , tInteraction = Grab "you grab the boat somehow."
        , tLabel = Label "boat"
        , tRoomDescription = Just $ "There is a " ++ Color.blue "boat" ++ " in the room."
        , tCombinations = Map.empty
        }
      door = Thing
        { tDescription = "A large, ornate, wooden door"
        , tInteraction = TravelRoom "you walk through the door" room2
        , tLabel = Label "door"
        , tRoomDescription = Just $ "To the north, there is a large, ornate, wooden " ++ Color.blue "door"
        , tCombinations = Map.empty
        }
      openBox = Thing
        { tDescription = "Theres a thimble in there"
        , tInteraction = Inspect "its just a box"
        , tLabel = Label "box"
        , tRoomDescription = Just $ "You see an open " ++ Color.blue "box" ++ " in the room"
        , tCombinations = Map.empty
        }
      thimble = Thing
        { tDescription = "its a thimble, in the box"
        , tInteraction = Grab "you grab the thimble"
        , tLabel = Label "thimble"
        , tRoomDescription = Nothing
        , tCombinations = Map.fromList [(Label "door", ActOnThing2 (Inspect "You throw your thimble at the door, and watch it bounce off and roll in small circles on the ground. You pick it back up, feeling a little bit silly..."))]
        }
      box = Thing
        { tDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
        , tInteraction = ReplaceSelfWithThings "You open the box." [openBox, thimble]
        , tLabel = Label "box"
        , tRoomDescription = Just $ "There is a " ++ Color.blue "box" ++ " in the corner of the room"
        , tCombinations = Map.empty
        }
      i = Map.fromList
        [ (tLabel boat, boat)
        , (tLabel box, box)
        , (tLabel door, door)
        ]
  in GameState { gRoom = room1
               , gYou = Map.empty
            , gTimeLeft = Time 10
            }
