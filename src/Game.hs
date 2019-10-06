module Game
    ( runGame
    ) where

import Util (prompt)
import Data.Maybe (catMaybes)


data Room =
  Room { description :: String
       , roomThings :: [Thing]
       }
  deriving Eq

data Thing =
  Thing { label :: String
        , thingDescription :: String
        }
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

  
data Action
  = Panic
  | Look
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
  let diffs = [ roomDiff oldState newState
              , timeDiff oldState newState
              ]
  in catMaybes diffs


parseInput :: String -> Action
parseInput input =
  case input of
    "panic" -> Panic
    "look" -> Look
    "help" -> Help
    "wait" -> Update NoOp
    _ -> BadInput


tickState :: GameState -> GameState
tickState state =
  let (Time t) = timeLeft state
  in state { timeLeft = Time (t - 1) }


updateState :: GameState -> UpdatingAction -> Either String GameState
updateState oldState action =
  Left "You sit around for a while"


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
        Panic ->
          putStrLn "bye!"
        Update update ->
          let updateResult = fmap tickState $ updateState oldState update
          in case updateResult of
               Right newState ->
                 do putStrLn . unlines $ showStateDiff oldState newState
                    loop newState
               Left message ->
                 putStrLn message >> loop oldState
        Help ->
          do putStrLn "commands: look, go north|south|east|west, panic, help"
             loop oldState
        BadInput ->
          putStrLn "huh?" >> loop oldState
    

initState :: GameState
initState =
  GameState { room = Room "the first room. There is a room to the north" []
            , timeLeft = Time 4
            }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
