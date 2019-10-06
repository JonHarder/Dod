module Game
    ( runGame
    ) where

import Util (prompt)


data Room =
  Room { description :: String
       , northRoom :: Maybe Room
       , southRoom :: Maybe Room
       }

instance Show Room where
  show = description


data State = State { panicking :: Bool, room :: Room }
  deriving Show


showStateDiff :: State -> State -> String
showStateDiff oldState newState =
  show newState


updateState :: State -> String -> State
updateState oldState input =
  if input == "panic"
  then
    oldState { panicking = True }
  else
    oldState


loop :: State -> IO ()
loop oldState = do
  input <- prompt "What do you wanna do: "
  let newState = updateState oldState input
  if panicking newState
    then putStrLn "bye!"
    else do
       putStrLn (showStateDiff oldState newState)
       loop newState
    

initState :: State
initState =
  let r = Room "the first room" Nothing Nothing
  in State { panicking = False, room = r }


runGame :: IO ()
runGame = do
  let state = initState
  putStrLn "welcome!"
  loop state
