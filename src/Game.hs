module Game
    ( runGame
    ) where

import Util (prompt)
import Data.Maybe (catMaybes)


data Room =
  Room { description :: String
       , northRoom :: Maybe Room
       , southRoom :: Maybe Room
       , eastRoom :: Maybe Room
       , westRoom :: Maybe Room
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


data Direction
  = North
  | South
  | East
  | West
  deriving Show


data UpdatingAction
  = Go Direction

  
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
    "go north" -> Update $ Go North -- use parsec to construct actions with strings
    "go south" -> Update $ Go South
    "go east" -> Update $ Go East
    "go west" -> Update $ Go West
    "help" -> Help
    _ -> BadInput


tickState :: GameState -> GameState
tickState state =
  let (Time t) = timeLeft state
  in state { timeLeft = Time (t - 1) }


updateState :: GameState -> UpdatingAction -> Either String GameState
updateState oldState action =
  case action of
    Go North ->
      case northRoom (room oldState) of
        Just newRoom ->
          Right $ oldState { room = newRoom }
        Nothing ->
          Left "you can't go north from here"
    Go South ->
      case southRoom (room oldState) of
        Just newRoom ->
          Right $ oldState { room = newRoom }
        Nothing ->
          Left "you can't go south from here"
    Go East ->
      case eastRoom (room oldState) of
        Just newRoom ->
          Right $ oldState { room = newRoom }
        Nothing ->
          Left "you can't go east from here"
    Go West ->
      case westRoom (room oldState) of
        Just newRoom ->
          Right oldState { room = newRoom }
        Nothing ->
          Left "you can't go west from here"


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
  let r = Room "the first room. There is a room to the north" Nothing Nothing Nothing Nothing
      r2 = Room "the second room. There is a room to the south" Nothing (Just r) Nothing Nothing
      finalRoom = r { northRoom = Just r2 }
  in GameState { room = finalRoom
               , timeLeft = Time 4
               }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
