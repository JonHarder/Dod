module InitState
  (initState)
  where

import Types
import GameState
import qualified Color

import qualified Data.Map.Strict as Map


initState :: GameState
initState =
  let boat = Thing
                 { tDescription = "There's a boat here, for some reason."
                 , tInteraction = Grab "you grab the boat somehow."
                 , tLabel = Label "boat"
                 , tRoomDescription = Just $ "There is a " ++ Color.red "boat" ++ " in the room."
                 }
      openBox = Thing
                { tDescription = "Theres a thimble in there"
                , tInteraction = Inspect "its just a box"
                , tLabel = Label "box"
                , tRoomDescription = Just $ "You see an open " ++ Color.red "box" ++ " in the room"
                }
      thimble = Thing
                { tDescription = "its a thimble, in the box"
                , tInteraction = Grab "you grab the thimble"
                , tLabel = Label "thimble"
                , tRoomDescription = Nothing
                }
      box = Thing
                 { tDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
                 , tInteraction = ReplaceSelfWithThings "You open the box." [openBox, thimble]
                 , tLabel = Label "box"
                 , tRoomDescription = Just $ "There is a " ++ Color.red "box" ++ " in the corner of the room"
                 }
      i = Map.fromList [(tLabel boat, boat), (tLabel box, box)]
  in GameState { gRoom = Room
                 { rShortDescription = "The first room, not much to do here as far as you can tell. Maybe you should " ++ Color.cyan "look" ++ " around a bit."
                 , rDescription = "A small dingey dungeon, with a kiddy pool in the center, and a weird table in the corner."
                 , rInventory = i
                 }
               , gYou = Map.empty
            , gTimeLeft = Time 10
            }
