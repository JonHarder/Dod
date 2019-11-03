module InitState
  (initState)
  where

import Types
import GameState

import qualified Data.Map.Strict as Map

initState :: GameState
initState =
  let boat = Thing
                 { tDescription = "There's a boat here, for some reason."
                 , tInteraction = Grab "you grab the boat somehow."
                 , tLabel = Label "boat"
                 , tRoomDescription = Nothing
                 }
      openBox = Thing
                { tDescription = "Theres a thimble in there"
                , tInteraction = Inspect "its just a box"
                , tLabel = Label "box"
                , tRoomDescription = Nothing
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
                 , tRoomDescription = Nothing
                 }
      i = Map.fromList [(tLabel boat, boat), (tLabel box, box)]
  in GameState { gRoom = Room
                 { rDescription = "the first room. boat? probably also a box"
                 , rInventory = i
                 }
               , gYou = Map.empty
            , gTimeLeft = Time 10
            }
