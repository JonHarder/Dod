module InitState
  (initState)
  where

import Types
import GameState

import qualified Data.Map.Strict as Map

initState :: GameState
initState =
  let boat = Thing
                 { thingDescription = "There's a boat here, for some reason."
                 , interaction = Grab "you grab the boat somehow."
                 , label = Label "boat"
                 , roomDescription = Nothing
                 }
      openBox = Thing
                { thingDescription = "Theres a thimble in there"
                , interaction = Inspect "its just a box"
                , label = Label "box"
                 , roomDescription = Nothing
                }
      thimble = Thing
                { thingDescription = "its a thimble, in the box"
                , interaction = Grab "you grab the thimble"
                , label = Label "thimble"
                , roomDescription = Nothing
                }
      box = Thing
                 { thingDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
                 , interaction = ReplaceSelfWithThings "You open the box." [openBox, thimble]
                 , label = Label "box"
                , roomDescription = Nothing
                 }
      i = Map.fromList [(label boat, boat), (label box, box)]
  in GameState { room = Room
                 { description = "the first room. boat? probably also a box"
                 , inventory = i
                 }
               , you = Map.empty
            , timeLeft = Time 10
            }
