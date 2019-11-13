{-# LANGUAGE OverloadedStrings #-}

module Stories.CryoSleep where

import GameState
import Color
import Data.Map.Strict as Map
import qualified Data.Text as T
import Types
import Stories.Types


initState :: GameState
initState =
  let roomCryoPod = Room
        { rShortDescription = T.concat ["You see the inside of a CryoPod 3001 that you are lieing down in. You think you might want to take a deeper ", green "look", "."]
        , rDescription = "In the tight space around you, you see lots of frost. You're lieing on a metal bed, with a cracked glass window in front of you. The window is covered in frost, so you can't see anything beyond it. You don't remember how you got here, and trying seems to hurt your head."
        , rInventory = Map.fromList [(tLabel thingButton, thingButton)]
        }
      thingButton = Thing
        { tDescription = "A large glowing button that says \"Open\", and is covered with a thin layer of frost"
        , tInteraction = TravelRoom "Pushing the button causes the glass to slide off the side, opening your Pod. You sit up, and immediately wich you hadn't as you try to avoid vomiting. You try to think back to what you were told about waking up from cryosleep, though only faint memories greet you, you're certain this is not what it's supposed to be like. Slowly, you coax your stiff joints into motion, and get out of your pod.\n\nNow that you're upright, and your vision is no longer obscured by the frosted glass, you begin to take in the room you in which you find yourself." roomCryoStorage
        , tLabel = Label "button"
        , tRoomDescription = Just $ T.concat ["To your right is a ", blue "button", ". If you ", green "interact", " with it the CryoPod should open."]
        , tCombinations = Map.empty
        }
      roomCryoStorage = Room
        { rShortDescription = T.concat ["You are in a small room with the ", blue "CryoPod", " that you woke up from. You should ", green "look", " around some more to see if you can find anything else in this room."]
         , rDescription = T.concat ["You ", green "look", " around the Cryo Storage room. You notice a bloody, dismemebered, ", blue "body", " in a heap. There is also a glowing blue card ", blue "scanner", " next to the door."]
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
        , tInteraction = GrabThings (T.concat ["You carefully dig through the body, avoiding touching more than you have to. You find a ", blue "keycard", " in the pockets of the body."]) [thingCryoKeyCard]
        , tLabel = Label "body"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
      thingCryoScanner = Thing
        { tDescription = T.concat ["You see a pedestal next to the ", blue "door", ". In the center of it's face, there is a slight indentation which is likely where a keycard would go.  Above the indentation, in bold, red lettering, you see the message \"Authorized Personelle only\""]
        , tInteraction = Inspect "You look around for any buttons, or barring that, a panel you can remove to try and short circuit the scanner, to no avail."
        , tLabel = Label "scanner"
        , tRoomDescription = Nothing
        , tCombinations = Map.empty
        }
      thingCryoKeyCard = Thing
        { tDescription = T.concat ["A key card with a magnetic strip. Perhaps you could ", green "use", " it ", green "on", " the ", blue "scanner", "."]
        , tInteraction = Describe
        , tLabel = Label "keycard"
        , tRoomDescription = Nothing
        , tCombinations = Map.fromList
          [ (tLabel thingCryoScanner, ActOnThing2 $ TriggerActionOn thingCryoStorageExitClosed $ ReplaceSelfWithThings "You place the keycard into the indentation in the scanner. You hear a faint ding and the door slides open." [thingCryoStorageExitOpened] )
          , (tLabel thingCryoStorageExitClosed, ActOnNothing $ T.concat ["Try using the ", blue "keycard", " on the ", blue "scanner", " instead."])
          ]
        }
      thingCryoStorageExitClosed = Thing
        { tDescription = "An automatic sliding metal door that is closed."
        , tInteraction = Inspect $ T.concat ["It doesn't seem like you'll be able to force it open. You'll have to gain acess through the card ", blue "scanner", "."]
        , tLabel = Label "door"
        , tRoomDescription = Just $ T.concat ["At the far end of the room, you see a large, metal ", blue "door", ", firmly shut"]
        , tCombinations = Map.empty
        }
      thingCryoStorageExitOpened = Thing
        { tDescription = "An automatic sliding metal door that is open."
        , tInteraction = TravelRoom "You step through the open door." roomHallway
        , tLabel = Label "door"
        , tRoomDescription = Just $ T.concat ["At the far end of the room, you see a large, open space, where the ", blue "door", " slid open."]
        , tCombinations = Map.empty
        }
      roomHallway = Room
        { rShortDescription = "A small hallway."
         , rDescription = "A small hallway adjoining your cryopod storage room, and the room ahead."
         , rInventory = Map.empty
         }
  in GameState
     { gRoom = roomCryoPod
     , gYou = Map.empty
     , gTimeLeft = Time 10
     , gEvents = Map.fromList [(Time 5, "Suddenly your knees buckle, and you stumble, your hand reaches out to the nearby wall to steady yourself.")]
     }

story :: Story
story = Story "This is a test story" initState
