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
import Control.Monad (liftM)
import Data.Either (fromRight)


restOfLine :: Parser String
restOfLine = manyTill anyToken eof

word :: Parser String
word = manyTill anyChar ((space >> return ()) <|> eof)


verb :: [String] -> a -> Parser a
verb s a = alias s >> eof >> return a


alias :: [String] -> Parser String
alias = choice . map string


unaryVerb :: [String] -> (String -> a) -> Parser a
unaryVerb s f = do
  _ <- alias s >> space
  liftM f restOfLine


binaryVerb :: String -> String -> (Label -> Label -> a) -> Parser a
binaryVerb action preposition f = do
  _ <- string action >> space
  label1 <- liftM Label word
  _ <- string preposition >> space
  label2 <- liftM Label word
  return $ f label1 label2


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


parseCombine :: Parser Action
parseCombine = binaryVerb "use" "on" (\l1 l2 -> Update (Combine l1 l2))


parseInteract :: Parser Action
parseInteract = unaryVerb ["open", "interact", "grab", "take"] $ Update . Interact . Label


parseInventory :: Parser Action
parseInventory = verb ["inventory", "i"] Inventory


parseAction :: Parser Action
parseAction =
      try parseLookAt
  <|> try parseInteract
  <|> parseCombine
  <|> parseLook
  <|> parseInventory
  <|> parsePanic
  <|> parseHelp
  <|> parseWait


parseInput :: String -> Action
parseInput =
  fromRight (BadInput Nothing) . parse parseAction ""
