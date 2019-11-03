module InputParser
  (parseInput)
  where


import Types (Label(..))
import Actions (Action(..), UpdatingAction(..))
import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , anyToken
  , choice
  , eof
  , manyTill
  , try
  , string
  , space
  , parse
  )
import Control.Monad (liftM)
import Data.Either (fromRight)


restOfLine :: Parser String
restOfLine = manyTill anyToken eof


verb :: [String] -> a -> Parser a
verb s a = alias s >> eof >> return a


alias :: [String] -> Parser String
alias = choice . map string


unaryVerb :: [String] -> (String -> a) -> Parser a
unaryVerb s f = do
  _ <- alias s >> space
  liftM f restOfLine


parseLook :: Parser Action
parseLook = verb ["look"] Look


parseLookAt :: Parser Action
parseLookAt = unaryVerb ["look"] $ LookAt . Label


parsePanic :: Parser Action
parsePanic = verb ["panic"] Panic


parseHelp :: Parser Action
parseHelp = verb ["help"] Help


parseWait :: Parser Action
parseWait = verb ["wait"] (Update NoOp)


parseInteract :: Parser Action
parseInteract = unaryVerb ["open", "interact", "grab", "take"] $ Update . Interact . Label


parseInventory :: Parser Action
parseInventory = verb ["inventory", "i"] Inventory


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
