module Game
    ( runGame
    ) where

import Util (prompt)
import Control.Monad (liftM)
import Control.Monad.State
import Data.Maybe (catMaybes, fromMaybe)
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (Parser, (<|>), anyToken, choice, eof, parse, manyTill, try, string, space)


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

data Label = Label String
  deriving (Eq, Ord)


instance Show Label where
  show (Label l) = l


type Inventory = (Map.Map Label Thing)

data GameState = GameState { you :: Inventory, room :: Room, timeLeft :: Time }
  deriving (Eq)


addToYou :: Thing -> GameState -> GameState
addToYou thing oldState =
  let oldYou = you oldState
  in oldState { you = Map.insert (label thing) thing oldYou }


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
  deriving (Eq, Show)


data UpdatingAction
  = NoOp
  | Interact Label
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


verb :: String -> a -> Parser a
verb s a = string s >> return a


alias :: [String] -> Parser String
alias = choice . map string


unaryVerb :: [String] -> (String -> a) -> Parser a
unaryVerb s f = do
  _ <- alias s >> space
  liftM f restOfLine


parseLook :: Parser Action
parseLook = verb "look" Look


parseLookAt :: Parser Action
parseLookAt = unaryVerb ["look"] $ LookAt . Label


parsePanic :: Parser Action
parsePanic = verb "panic" Panic


parseHelp :: Parser Action
parseHelp = verb "help" Help


parseWait :: Parser Action
parseWait = verb "wait" (Update NoOp)


parseInteract :: Parser Action
parseInteract = unaryVerb ["interact", "grab"] $ Update . Interact . Label


parseInventory :: Parser Action
parseInventory = verb "inventory" Inventory


parseAction :: Parser Action
parseAction =
      try parseLookAt
  <|> try parseInteract
  <|> parseLook
  <|> parseInventory
  <|> parsePanic
  <|> parseHelp
  <|> parseWait


parseInput :: String -> Action
parseInput =
  fromRight (BadInput Nothing) . parse parseAction ""


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


dispatchThingAction :: Thing -> ThingAction -> State GameState UpdateResult
dispatchThingAction thing action = do
  case action of
    Grab msg -> do
      modify $ addToYou thing
      modify $ removeFromRoom thing
      newState <- get
      return $ ChangedState newState msg
    Inspect msg ->
      get >>= \oldState -> return $ ChangedState oldState msg


updateStateWithThing :: GameState -> Thing -> ThingAction -> UpdateResult
updateStateWithThing oldState thing action =
  let (updateResult, _) = runState (dispatchThingAction thing action) oldState
  in updateResult


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
      box = Thing
                 { thingDescription = "You see a box, with a poorly designed lid, propped slightly open. You can't quite make out what's inside."
                 , interaction = Inspect "You open the box."
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
