module Game
    ( runGame
    ) where

import Util (prompt, (|>))
import Actions (Action(..), Label(..), UpdatingAction(..), parseInput)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as Map


data Room =
  Room { description :: String
       , inventory :: Inventory
       }
  deriving Eq

data Thing =
  Thing { thingDescription :: String
        , interaction :: ThingAction
        , label :: Label
        }

instance Eq Thing where
  t1 == t2 = thingDescription t1 == thingDescription t2 && label t1 == label t2



type Inventory = (Map.Map Label Thing)

data GameState = GameState { you :: Inventory, room :: Room, timeLeft :: Time }
  deriving (Eq)


addToYou :: Thing -> GameState -> GameState
addToYou thing oldState =
  let oldYou = you oldState
  in oldState { you = Map.insert (label thing) thing oldYou }


addToRoom :: Thing -> GameState -> GameState
addToRoom thing oldState =
  let newRoom = (room oldState) { inventory = Map.insert (label thing) thing $ roomInventory oldState }
  in oldState { room = newRoom }


newtype Time = Time Int
  deriving (Eq, Ord)


instance Show Time where
  show (Time t) = "Time left: " ++ show t


instance Show Room where
  show = description

instance Show Thing where
  show = thingDescription


data ThingAction
  = Grab String
  | Inspect String
  | ReplaceSelfWithThings String [Thing]
  deriving (Eq, Show)



-- https://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Combinator.html



data UpdateResult
  = NoChangeWithMessage String
  | ChangedState GameState String
  | Terminate String


roomDiff :: GameState -> GameState -> Maybe String
roomDiff oldState newState =
  if room oldState /= room newState
    then Just $ show $ room newState
    else Nothing


timeDiff :: GameState -> GameState -> Maybe String
timeDiff oldState newState =
  if timeLeft oldState /= timeLeft newState
    then Just $ show $ timeLeft newState
  else
    Nothing


showStateDiff :: GameState -> GameState -> [String]
showStateDiff oldState newState =
  let funcs = [ roomDiff, timeDiff ]
      diffs = fmap (\f -> f oldState newState) funcs
  in catMaybes diffs


tickState :: GameState -> GameState
tickState oldState =
  let (Time t) = timeLeft oldState
  in oldState { timeLeft = Time (t - 1) }


roomInventory :: GameState -> Inventory
roomInventory = inventory . room


findInInventory :: Label -> Inventory -> Maybe Thing
findInInventory = Map.lookup


removeFromRoom :: Thing -> GameState -> GameState
removeFromRoom thing oldState =
  let newInv = Map.delete (label thing) (roomInventory oldState)
      newRoom = (room oldState) { inventory = newInv }
  in oldState { room = newRoom }


updateStateWithThing :: GameState -> Thing -> ThingAction -> UpdateResult
updateStateWithThing oldState thing action =
  case action of
    Grab msg ->
      let newState = oldState
                      |> addToYou thing
                      |> removeFromRoom thing
      in ChangedState newState msg
    Inspect msg ->
      ChangedState oldState msg
    ReplaceSelfWithThings msg things ->
      let newState = oldState
                       |> removeFromRoom thing
                       |> \gameState-> foldl (flip addToRoom) gameState things
      in ChangedState newState msg


updateState :: GameState -> UpdatingAction -> UpdateResult
updateState oldState action =
  case action of
    NoOp ->
      ChangedState oldState "you do nothing for a bit"
    Interact l ->
      case findInInventory l (roomInventory oldState) of
        Nothing ->
          NoChangeWithMessage "couldn't find that here"
        Just thing ->
          updateStateWithThing oldState thing (interaction thing)


lookAt :: Label -> Inventory -> String
lookAt thingLabel i =
  maybe ("couldn't find " ++ show thingLabel ++ " here.") show (findInInventory thingLabel i)


dispatchAction :: GameState -> Action -> UpdateResult
dispatchAction oldState action =
  case action of
    Look ->
      NoChangeWithMessage $ show (room oldState)
    LookAt thing ->
      NoChangeWithMessage $ lookAt thing (roomInventory oldState)
    Inventory ->
      NoChangeWithMessage $ "you have: " ++ show (Map.keys (you oldState))
    Panic ->
      Terminate "you flip the fluff out"
    Update updatingAction ->
      updateState oldState updatingAction
    Help ->
      NoChangeWithMessage "commands: look, interact, wait, help, panic"
    BadInput msg ->
      NoChangeWithMessage $ fromMaybe "huh?" msg


loop :: GameState -> IO ()
loop oldState
  | timeLeft oldState <= Time 0 =
      putStrLn "Times up! You died."
  | otherwise = do
      action <- fmap parseInput $ prompt "What do you wanna do: "
      case dispatchAction oldState action of
        NoChangeWithMessage msg ->
          putStrLn msg >> loop oldState
        ChangedState newState message -> do
          let newState' = tickState newState
              stateDiff = showStateDiff oldState newState'
              messages = unlines $ message:stateDiff
          putStrLn messages
          loop newState'
        Terminate msg ->
          putStrLn msg
    

initState :: GameState
initState =
  let boat = Thing
                 { thingDescription = "There's a boat here, for some reason."
                 , interaction = Grab "you grab the boat somehow."
                 , label = Label "boat"
                 }
      openBox = Thing
                { thingDescription = "Theres a thimble in there"
                , interaction = Inspect "its just a box"
                , label = Label "box"
                }
      thimble = Thing
                { thingDescription = "its a thimble, in the box"
                , interaction = Grab "you grab the thimble"
                , label = Label "thimble"}
      box = Thing
                 { thingDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
                 , interaction = ReplaceSelfWithThings "You open the box." [openBox, thimble]
                 , label = Label "box"
                 }
      i = Map.fromList [(label boat, boat), (label box, box)]
  in GameState { room = Room
                 { description = "the first room. boat? probably also a box"
                 , inventory = i
                 }
               , you = Map.empty
            , timeLeft = Time 10
            }


runGame :: IO ()
runGame = do
  putStrLn "welcome!"
  loop initState
