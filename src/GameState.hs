module GameState where

import qualified Data.Map.Strict as Map

import Types

data Room =
  Room { rShortDescription :: String
       , rDescription :: String
       , rInventory :: Inventory
       }
  deriving Eq


instance Show Room where
  show = rShortDescription


data GameState = GameState
  { gYou :: Inventory
  , gRoom :: Room
  , gTimeLeft :: Time
  }
  deriving (Eq)

type Inventory = (Map.Map Label Thing)


data Thing =
  Thing { tDescription :: String
        , tInteraction :: ThingAction
        , tCombinations :: Map.Map Label MultiThingAction
        , tLabel :: Label
        , tRoomDescription :: Maybe String
        }

instance Eq Thing where
  t1 == t2 = tDescription t1 == tDescription t2 && tLabel t1 == tLabel t2

instance Show Thing where
  show = tDescription


data MultiThingAction
  = ActOnThing1 ThingAction
  | ActOnThing2 ThingAction
  deriving (Eq)


data ThingAction
  = Grab String
  | Inspect String
  | ReplaceSelfWithThings String [Thing]
  | TravelRoom String Room
  | Describe
  | GrabThings String [Thing]
  | TriggerActionOn Thing ThingAction
  deriving (Eq, Show)


addToYou :: Thing -> GameState -> GameState
addToYou thing oldState =
  let oldYou = gYou oldState
      newInventory = Map.insert (tLabel thing) thing oldYou
  in oldState { gYou = newInventory }


roomInventory :: GameState -> Inventory
roomInventory = rInventory . gRoom


addToRoom :: Thing -> GameState -> GameState
addToRoom thing oldState =
  let newRoom = (gRoom oldState) { rInventory = Map.insert (tLabel thing) thing $ roomInventory oldState }
  in oldState { gRoom = newRoom }


findInInventory :: Label -> Inventory -> Maybe Thing
findInInventory = Map.lookup


removeFromRoom :: Thing -> GameState -> GameState
removeFromRoom thing oldState =
  let thingLabel = tLabel thing
      newInv = Map.delete thingLabel (roomInventory oldState)
      newRoom = (gRoom oldState) { rInventory = newInv }
  in oldState { gRoom = newRoom }
