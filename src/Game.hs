module Game
    ( runGame
    ) where

import Util (prompt)


data Room =
  Room { description :: String
       , northRoom :: Maybe Room
       , southRoom :: Maybe Room
       , eastRoom :: Maybe Room
       , westRoom :: Maybe Room
       }
  deriving Eq

instance Show Room where
  show = description


data GameState = GameState { room :: Room, message :: Maybe String }
  deriving (Eq)


class Look a where
  look :: a -> String


instance Look Room where
  look = description


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
  | BadInput

  

showStateDiff :: GameState -> GameState -> [String]
showStateDiff oldState newState =
  let output = []
      outputWithMessage = case message newState of
        Just m ->
          m : output
        Nothing ->
          output
      outputWithRoom = if room oldState /= room newState
        then (look $ room newState) : outputWithMessage
        else outputWithMessage
  in outputWithRoom


parseInput :: String -> Action
parseInput input =
  case input of
    "panic" -> Panic
    "look" -> Look
    "go north" -> Update $ Go North -- use parsec to construct actions with strings
    "go south" -> Update $ Go South
    "go east" -> Update $ Go East
    "go west" -> Update $ Go West
    _ -> BadInput


updateState :: GameState -> UpdatingAction -> GameState
updateState oldState action =
  case action of
    Go North ->
      case northRoom (room oldState) of
        Just newRoom ->
          oldState { room = newRoom }
        Nothing ->
          oldState { message = Just "you can't go north from here" }
    Go South ->
      case southRoom (room oldState) of
        Just newRoom ->
          oldState { room = newRoom }
        Nothing ->
          oldState { message = Just "you can't go south from here" }
    Go East ->
      case eastRoom (room oldState) of
        Just newRoom ->
          oldState { room = newRoom }
        Nothing ->
          oldState { message = Just "you can't go east from here" }
    Go West ->
      case westRoom (room oldState) of
        Just newRoom ->
          oldState { room = newRoom }
        Nothing ->
          oldState { message = Just "you can't go west from here" }


resetMessage :: GameState -> GameState
resetMessage state = state { message = Nothing }
  

loop :: GameState -> IO ()
loop oldState = do
  input <- prompt "What do you wanna do: "
  let action = parseInput input
  case action of
    Look ->
      do putStrLn (look $ room oldState)
         loop oldState
    Panic ->
      putStrLn "bye!"
    Update update ->
      let newState = updateState oldState update
      in do putStrLn $ unlines $ showStateDiff oldState newState
            loop $ resetMessage newState
    BadInput ->
      putStrLn "huh?" >> loop oldState
    

initState :: GameState
initState =
  let r = Room "the first room. There is a room to the north" Nothing Nothing Nothing Nothing
      r2 = Room "the second room. There is a room to the south" Nothing (Just r) Nothing Nothing
      finalRoom = r { northRoom = Just r2 }
  in GameState { room = finalRoom, message = Nothing }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
