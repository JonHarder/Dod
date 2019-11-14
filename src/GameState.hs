{-# LANGUAGE OverloadedStrings #-}

module GameState where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))

import Types

data Room =
  Room { rShortDescription :: String
       , rDescription :: String
       , rInventory :: Inventory
       }
  deriving Eq

instance FromJSON Room where
  parseJSON (Y.Object v) =
    Room <$>
    v .: "short-description" <*>
    v .: "description" <*>
    v .: "inventory"
  parseJSON _ =
    fail "Room must be given an Object"


instance Show Room where
  show = rShortDescription


type Event = String

data GameState = GameState
  { gYou :: Inventory
  , gRoom :: Room
  , gTimeLeft :: Time
  , gEvents :: Map Time Event
  }
  deriving (Eq, Show)


instance FromJSON GameState where
  parseJSON (Y.Object v) =
    GameState <$>
    v .: "inventory" <*>
    v .: "room" <*>
    v .: "time" <*>
    v .: "events"
  parseJSON _ =
    fail "GameState must be given an Object"


currentEvent :: GameState -> Maybe Event
currentEvent gameState =
  Map.lookup (gTimeLeft gameState) (gEvents gameState)

type Inventory = (Map Label Thing)


data Thing =
  Thing { tDescription :: String
        , tInteraction :: ThingAction
        , tCombinations :: Map Label MultiThingAction
        , tLabel :: Label
        , tRoomDescription :: Maybe String
        }

instance FromJSON Thing where
  parseJSON (Y.Object v) =
    Thing <$>
    v .: "description" <*>
    v .: "interaction" <*>
    v .: "combinations" <*>
    (Label <$> v .: "label") <*>
    v .: "room-description"
  parseJSON _ =
    fail "Expected object for Thing value"

instance Eq Thing where
  t1 == t2 = tDescription t1 == tDescription t2 && tLabel t1 == tLabel t2

instance Show Thing where
  show = tDescription


data MultiThingAction
  = ActOnThing1 ThingAction
  | ActOnThing2 ThingAction
  | ActOnNothing String
  deriving (Eq, Show)


instance FromJSON MultiThingAction where
  parseJSON (Y.Object v) = do
    actOn <- v .: "act-on" :: Y.Parser String
    case actOn of
      "thing1" ->
        ActOnThing1 <$> v .: "thing-action"
      "thing2" ->
        ActOnThing2 <$> v .: "thing-action"
      "nothing" ->
        ActOnNothing <$> v .: "message"
      _ -> fail "invalid value for act-on when parsing MultiThingAction"
  parseJSON _ =
    fail "MultiThing must be given an object"



data ThingAction
  = Grab String
  | Inspect String
  | ReplaceSelfWithThings String [Thing]
  | TravelRoom String Room
  | Describe
  | GrabThings String [Thing]
  | TriggerActionOn Thing ThingAction
  deriving (Eq, Show)


instance FromJSON ThingAction where
  parseJSON (Y.Object v) = do
    phrase <- v .: "action" :: Y.Parser String
    case phrase of
      "grab" ->
        Grab <$> v .: "thing"
      "inspect" ->
        Inspect <$> v .: "thing"
      "replace" ->
        ReplaceSelfWithThings <$> v .: "message" <*> v .: "things"
      "travel" ->
        TravelRoom <$> v .: "message" <*> v .: "room"
      "describe" ->
        return Describe
      "grab-things" ->
        GrabThings <$> v .: "message" <*> v .: "things"
      "trigger-action" ->
        TriggerActionOn <$> v .: "thing" <*> v .: "on"
      _ ->
        fail $ "ThingAction: " ++ phrase ++ " not implemented yet."
  parseJSON _ =
    fail "ThingAction was expecting an Object"
    


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
