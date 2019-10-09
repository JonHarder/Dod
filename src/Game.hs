module Game
    ( runGame
    ) where

import Util (prompt, printMaybe, maybeHead)
import Data.Maybe (catMaybes)
import Data.String.Utils (startswith)


data Room =
  Room { description :: String
       , inventory :: Inventory
       }
  deriving Eq

data Thing =
  Thing { label :: String
        , thingDescription :: String
        , interaction :: UpdatingAction
        }
  deriving Eq


newtype Inventory = Inventory [Thing]
  deriving Eq


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
  | Interact String
  deriving Eq

data Action
  = Panic
  | Look
  | LookAt String
  | Update UpdatingAction
  | Help
  | BadInput


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
  | startswith "interact " input = Update $ Interact $ drop 9 input
  | startswith "look " input = LookAt $ drop 5 input
  | otherwise = BadInput


tickState :: GameState -> GameState
tickState state =
  let (Time t) = timeLeft state
  in state { timeLeft = Time (t - 1) }


roomInventory :: GameState -> Inventory
roomInventory = inventory . room

updateState :: GameState -> UpdatingAction -> Either String (GameState, Maybe String)
updateState oldState action =
  case action of
    NoOp ->
      Right (oldState, Just "you do nothing for a bit")
    Inspect msg ->
      Right (oldState, Just msg)
    Interact l ->
      case findInInventory l (roomInventory oldState) of
        Nothing ->
          Left "couldn't find that here"
        Just thing ->
          updateState oldState (interaction thing)



findInInventory :: String -> Inventory -> Maybe Thing
findInInventory searchLabel (Inventory things) =
  maybeHead $ filter (\thing -> label thing == searchLabel) things


loop :: GameState -> IO ()
loop oldState
  | timeLeft oldState <= Time 0 =
      putStrLn "Times up! You died."
  | otherwise = do
      input <- prompt "What do you wanna do: "
      let action = parseInput input
      case action of
        Look ->
          putStrLn (show $ room oldState) >> loop oldState
        LookAt thing ->
          do case findInInventory thing (inventory $ room oldState) of
               Just found ->
                 print found
               Nothing ->
                 putStrLn "Couldn't find any of those here."
             loop oldState
        Panic ->
          putStrLn "bye!"
        Update update ->
          let updateResult = updateState oldState update
          in case updateResult of
               Right (newState, message) ->
                 do let newState' = tickState newState
                    printMaybe message
                    putStrLn . unlines $ showStateDiff oldState newState'
                    loop newState'
               Left message ->
                 putStrLn message >> loop oldState
        Help ->
          do putStrLn "commands: look, go north|south|east|west, panic, help"
             loop oldState
        BadInput ->
          putStrLn "huh?" >> loop oldState
    

initState :: GameState
initState =
  GameState { room = Room
                 { description = "the first room. boat?"
                 , inventory = Inventory
                      [ Thing { label = "boat"
                              , thingDescription = "there's a boat here, for some reason."
                              , interaction = Inspect "It seriously doesn't make any sense, it's just a boat... in the middle of the room..."
                              
                              }
                      ]
                 }
            , timeLeft = Time 9999
            }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
