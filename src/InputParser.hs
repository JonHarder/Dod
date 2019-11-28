module InputParser
  (parseInput)
  where


import Types (Label(..))
import Actions (Action(..), UpdatingAction(..))
import Text.ParserCombinators.Parsec
  ( Parser
  , (<|>)
  , anyToken
  , anyChar
  , choice
  , eof
  , manyTill
  , try
  , string
  , space
  , parse
  )
import Control.Monad (void)
import Data.Either (fromRight)


restOfLine :: Parser String
restOfLine = manyTill anyToken eof

word :: Parser String
word = manyTill anyChar (void space <|> eof)


verb :: [String] -> a -> Parser a
verb s a = alias s >> eof >> return a


alias :: [String] -> Parser String
alias = choice . map (try . string)


unaryVerb :: [String] -> (String -> a) -> Parser a
unaryVerb s f = do
  _ <- alias s >> space
  f <$> restOfLine


binaryVerb :: [String] -> String -> (Label -> Label -> a) -> Parser a
binaryVerb action preposition f = do
  _ <- alias action >> space
  label1 <- Label <$> word
  _ <- string preposition >> space
  label2 <- Label <$> word
  return $ f label1 label2


parseLook :: Parser Action
parseLook = do
  _ <- alias ["look", "ls"]
  void (space >> string "around") <|> eof
  return Look


-- |Removes the first instance found from the list of strings given
strip :: [String] -> Parser ()
strip aliases =
  try (alias aliases >> space >> return ()) <|> return ()


parseLookAt :: Parser Action
parseLookAt = do
  string "look" >> space
  strip ["at", "for"] >> strip ["a", "the"]
  LookAt . Label <$> restOfLine


parseTell :: Parser Action
parseTell = do
  _ <- alias ["ask", "say", "tell"] >> space
  Tell . Label <$> word <*> restOfLine


parsePanic :: Parser Action
parsePanic = verb ["panic"] Panic


parseHelp :: Parser Action
parseHelp = verb ["help"] Help


parseWait :: Parser Action
parseWait = verb ["wait"] (Update NoOp)


parseCombine :: Parser Action
parseCombine = binaryVerb ["use"] "on" (\l1 l2 -> Update (Combine l1 l2))


parseInteract :: Parser Action
parseInteract = do
  alias ["push", "use", "press", "open", "interact", "grab", "take"]
  space >> strip ["the"]
  Update . Interact . Label <$> restOfLine


parseInventory :: Parser Action
parseInventory = verb ["inventory", "i"] Inventory


parseAction :: Parser Action
parseAction =
  try parseLook
  <|> parseLookAt
  <|> try parseCombine
  <|> try parseInteract
  <|> parseInventory
  <|> parsePanic
  <|> parseHelp
  <|> parseWait
  <|> parseTell


parseInput :: String -> Action
parseInput input =
  case parse parseAction "" input of
    Left err ->
      BadInput input
    Right action ->
      action
