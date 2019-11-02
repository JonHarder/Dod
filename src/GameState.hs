module GameState where

import qualified Data.Map.Strict as Map

import Types

data Room =
  Room { description :: String
       , inventory :: Inventory
       }
  deriving Eq


instance Show Room where
  show = description


data GameState = GameState { you :: Inventory, room :: Room, timeLeft :: Time }
  deriving (Eq)

type Inventory = (Map.Map Label Thing)


data Thing =
  Thing { thingDescription :: String
        , interaction :: ThingAction
        , label :: Label
        }

instance Eq Thing where
  t1 == t2 = thingDescription t1 == thingDescription t2 && label t1 == label t2

instance Show Thing where
  show = thingDescription


data ThingAction
  = Grab String
  | Inspect String
  | ReplaceSelfWithThings String [Thing]
  deriving (Eq, Show)


addToYou :: Thing -> GameState -> GameState
addToYou thing oldState =
  let oldYou = you oldState
  in oldState { you = Map.insert (label thing) thing oldYou }


roomInventory :: GameState -> Inventory
roomInventory = inventory . room


addToRoom :: Thing -> GameState -> GameState
addToRoom thing oldState =
  let newRoom = (room oldState) { inventory = Map.insert (label thing) thing $ roomInventory oldState }
  in oldState { room = newRoom }


findInInventory :: Label -> Inventory -> Maybe Thing
findInInventory = Map.lookup


removeFromRoom :: Thing -> GameState -> GameState
removeFromRoom thing oldState =
  let newInv = Map.delete (label thing) (roomInventory oldState)
      newRoom = (room oldState) { inventory = newInv }
  in oldState { room = newRoom }
