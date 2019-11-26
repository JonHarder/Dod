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
  _ <- string "look"
  void (space >> string "around") <|> eof
  return Look


parseLookAt :: Parser Action
parseLookAt = unaryVerb ["look", "l"] $ LookAt . Label


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
parseInteract = unaryVerb ["press", "open", "interact", "grab", "take"] $ Update . Interact . Label


parseInventory :: Parser Action
parseInventory = verb ["inventory", "i"] Inventory


parseAction :: Parser Action
parseAction =
  try parseLook
  <|> parseLookAt
  <|> parseInteract
  <|> parseCombine
  <|> parseInventory
  <|> parsePanic
  <|> parseHelp
  <|> parseWait
  <|> parseTell


parseInput :: String -> Action
parseInput =
  fromRight (BadInput Nothing) . parse parseAction ""
