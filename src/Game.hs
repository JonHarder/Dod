module Game
    ( runGame
    ) where

import Util (prompt, printMaybe, printLines)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.String.Utils (startswith)


data Room =
  Room { description :: String
       , inventory :: Inventory
       }
  deriving Eq

data Thing =
  Thing { thingDescription :: String
        , interaction :: UpdatingAction
        }
  deriving Eq

data Label = Label String
  deriving (Eq, Ord)


type Inventory = (Map.Map Label Thing)

data GameState = GameState { room :: Room, timeLeft :: Time }
  deriving (Eq)


newtype Time = Time Int
  deriving (Eq, Ord)


instance Show Time where
  show (Time t) = "Time left: " ++ show t


instance Show Room where
  show = description

instance Show Thing where
  show = thingDescription


data UpdatingAction
  = NoOp
  | Inspect String
  | Interact Label
  deriving Eq


data Action
  = Panic
  | Look
  | LookAt Label
  | Update UpdatingAction
  | Help
  | BadInput (Maybe String)


data UpdateResult
  = NoChangeWithMessage String
  | ChangeWithMessage GameState String
  | ChangeWithNoMessage GameState
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


parseInput :: String -> Action
parseInput input
  | input == "panic" = Panic
  | input == "look" = Look
  | input == "help" = Help
  | input == "wait" = Update NoOp
  | input == "interact" = BadInput (Just "what do you want to interact with?")
  | startswith "interact " input = Update $ Interact (Label $ drop 9 input)
  | startswith "look " input = LookAt $ (Label $ drop 5 input)
  | otherwise = BadInput Nothing


tickState :: GameState -> GameState
tickState state =
  let (Time t) = timeLeft state
  in state { timeLeft = Time (t - 1) }


roomInventory :: GameState -> Inventory
roomInventory = inventory . room


updateState :: GameState -> UpdatingAction -> UpdateResult
updateState oldState action =
  case action of
    NoOp ->
      ChangeWithMessage oldState "you do nothing for a bit"
    Inspect msg ->
      ChangeWithMessage oldState msg
    Interact l ->
      case findInInventory l (roomInventory oldState) of
        Nothing ->
          NoChangeWithMessage "couldn't find that here"
        Just thing ->
          updateState oldState (interaction thing)



findInInventory :: Label -> Inventory -> Maybe Thing
findInInventory = Map.lookup


lookAt :: Label -> Inventory -> String
lookAt thingLabel i =
  maybe "couldn't find any of those here" show (findInInventory thingLabel i)


dispatchAction :: GameState -> Action -> UpdateResult
dispatchAction state action =
  case action of
    Look ->
      NoChangeWithMessage $ show (room state)
    LookAt thing ->
      NoChangeWithMessage $ lookAt thing (roomInventory state)
    Panic ->
      Terminate "you flip the fluff out"
    Update updatingAction ->
      updateState state updatingAction
    Help ->
      NoChangeWithMessage "commands: look, interact, wait, help, panic"
    BadInput msg ->
      NoChangeWithMessage $ fromMaybe "huh?" msg


stateDelta :: GameState -> GameState -> Maybe String -> (GameState, String)
stateDelta oldState newState maybeMessage =
  let newState' = tickState newState
      stateDiff = showStateDiff oldState newState'
      messages = maybe stateDiff (:stateDiff) maybeMessage
  in (newState', unlines messages)


loop :: GameState -> IO ()
loop oldState
  | timeLeft oldState <= Time 0 =
      putStrLn "Times up! You died."
  | otherwise = do
      action <- fmap parseInput $ prompt "What do you wanna do: "
      case dispatchAction oldState action of
        NoChangeWithMessage msg ->
          putStrLn msg >> loop oldState
        ChangeWithMessage newState msg ->
          do
            let (tickedState, messages) = stateDelta oldState newState (Just msg)
            putStrLn messages
            loop tickedState
        ChangeWithNoMessage newState ->
          do
            let (tickedState, messages) = stateDelta oldState newState Nothing
            putStrLn messages
            loop tickedState
        Terminate msg ->
          putStrLn msg
    

initState :: GameState
initState =
  let boat = (Label "boat"
             , Thing
                 { thingDescription = "There's a boat here, for some reason."
                 , interaction = Inspect "It seriously doesn't make any ense, it's just a boat."
                 }
             )
      i = Map.fromList [boat]
  in GameState { room = Room
                 { description = "the first room. boat? probably also a box"
                 , inventory = i
                 }
            , timeLeft = Time 10
            }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
