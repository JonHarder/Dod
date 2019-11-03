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
         , rDescription = "As you take a deeper look around the Cryo Storage room, you notice a bloody, dismemebered, " ++ Color.blue "body" ++ " in a heep just inside a closed " ++ Color.blue "door" ++ ". There is also a glowing blue card " ++ Color.blue "scanner" ++ " next to the door."
         , rInventory = Map.fromList [(tLabel thingCryoPod, thingCryoPod), (tLabel thingCryoBody, thingCryoBody), (tLabel thingCryoStorageExitClosed, thingCryoStorageExitClosed), (tLabel thingCryoScanner, thingCryoScanner)]
         }
      thingCryoPod = Thing
        { tDescription = "The CryoPod 3001 that you woke up from."
        , tInteraction = Inspect "The CryoPod seems familiar, but there doesn't seem to be anything left to discover here."
        , tLabel = Label "CryoPod"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
      thingCryoBody = Thing
        { tDescription = "A smelly, blood covered pile of body parts."
        , tInteraction = Inspect "The CryoPod seems familiar, but there doesn't seem to be anything left to discover here."
        , tLabel = Label "body"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
      thingCryoScanner = Thing
        { tDescription = "An automatic sliding metal door."
        , tInteraction = Inspect $ "A solid metal door. It doesn't seem like you'll be able to force it open. You'll have to gain acess through the card " ++ Color.blue "scanner" ++ "."
        , tLabel = Label "door"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
      thingCryoKeyCard = Thing
        { tDescription = "A key card with a magnetic strip."
        , tInteraction = Describe
        , tLabel = Label "door"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
      thingCryoStorageExitClosed = Thing
        { tDescription = "An automatic sliding metal door."
        , tInteraction = Inspect $ "A solid metal door. It doesn't seem like you'll be able to force it open. You'll have to gain acess through the card " ++ Color.blue "scanner" ++ "."
        , tLabel = Label "door"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
  in GameState { gRoom = roomCryoPod
               , gYou = Map.empty
            , gTimeLeft = Time 10
            }

story :: Story
story = Story "This is a test story" initState
