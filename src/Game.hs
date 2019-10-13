module Game
    ( runGame
    ) where

import Util (prompt)
import Control.Monad (liftM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (Parser, (<|>), anyToken, eof, parse, manyTill, try, string, space)


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


instance Show Label where
  show (Label l) = l


type Inventory = (Map.Map Label Thing)

data GameState = GameState { you :: Inventory, room :: Room, timeLeft :: Time }
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
  | Grab Label
  deriving (Eq, Show)


data Action
  = Panic
  | Look
  | LookAt Label
  | Inventory
  | Update UpdatingAction
  | Help
  | BadInput (Maybe String)
  deriving (Show)

-- https://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Combinator.html

restOfLine :: Parser String
restOfLine = manyTill anyToken eof


unary :: String -> a -> Parser a
unary s a = string s >> return a


binary :: String -> (String -> a) -> Parser a
binary s f = do
  _ <- string s >> space
  liftM f restOfLine

parseLook :: Parser Action
parseLook = unary "look" Look


parseLookAt :: Parser Action
parseLookAt = binary "look" $ LookAt . Label


parsePanic :: Parser Action
parsePanic = unary "panic" Panic


parseHelp :: Parser Action
parseHelp = unary "help" Help


parseWait :: Parser Action
parseWait = unary "wait" (Update NoOp)


parseInteract :: Parser Action
parseInteract = binary "interact" $ Update . Interact . Label


parseGrab :: Parser Action
parseGrab = binary "grab" $ Update . Grab . Label


parseInventory :: Parser Action
parseInventory = unary "inventory" Inventory


parseAction :: Parser Action
parseAction =
      try parseLookAt
  <|> try parseInteract
  <|> parseLook
  <|> parseGrab
  <|> parseInventory
  <|> parsePanic
  <|> parseHelp
  <|> parseWait


parseInput :: String -> Action
parseInput =
  fromRight (BadInput Nothing) . parse parseAction ""


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
    Grab l ->
      case findInInventory l (roomInventory oldState) of
        Nothing ->
          NoChangeWithMessage $ "There aren't any " ++ show l ++ " around to grab"
        Just thing ->
          let addedToYou = oldState { you = Map.insert l thing $ you oldState }
              gameRoom = room addedToYou
              gameRoomInventory = inventory gameRoom
              removedRoomInventory = Map.delete l gameRoomInventory
              updatedRoom = addedToYou { room = gameRoom { inventory = removedRoomInventory } }
          in ChangeWithMessage updatedRoom $ "You grab the " ++ show l


findInInventory :: Label -> Inventory -> Maybe Thing
findInInventory = Map.lookup


lookAt :: Label -> Inventory -> String
lookAt thingLabel i =
  maybe ("couldn't find " ++ show thingLabel ++ " here.") show (findInInventory thingLabel i)


dispatchAction :: GameState -> Action -> UpdateResult
dispatchAction state action =
  case action of
    Look ->
      NoChangeWithMessage $ show (room state)
    LookAt thing ->
      NoChangeWithMessage $ lookAt thing (roomInventory state)
    Inventory ->
      NoChangeWithMessage $ "you have: " ++ show (Map.keys (you state))
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
                 , interaction = Inspect "It seriously doesn't make any sense, it's just a boat."
                 }
             )
      box = ( Label "box"
            , Thing
                 { thingDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
                 , interaction = Inspect "You open the box."
                 }
            )
      i = Map.fromList [boat, box]
  in GameState { room = Room
                 { description = "the first room. boat? probably also a box"
                 , inventory = i
                 }
               , you = Map.empty
            , timeLeft = Time 10
            }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
