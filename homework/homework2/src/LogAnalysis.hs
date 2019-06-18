module LogAnalysis where

import Data.Char
  ( digitToInt
  )

import Data.List
  ( foldl'
  )

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec
  ( (<|>)
  )

import Log
  ( LogMessage(LogMessage, Unknown)
  , MessageType(Info, Warning, Error)
  , TimeStamp
  , MessageTree(Leaf, Node)
  )

parse' :: String -> Either Parsec.ParseError [LogMessage]
parse' = Parsec.parse parseMessage' "(unknown)"

parse :: String -> [LogMessage]
parse s =
  case parse' s of
    Left err -> [LogMessage (Error 1000) 0 (show err)]
    Right msgs -> msgs


parseMessage' :: Parsec.GenParser Char st [LogMessage]
parseMessage' = do
  result <- Parsec.many logMessage'
  Parsec.eof
  return result

eol :: Parsec.GenParser Char st Char
eol = Parsec.char '\n'

logMessage' :: Parsec.GenParser Char st LogMessage
logMessage' = do
  messageType <- logMessageType'
  Parsec.spaces
  messageTimestamp <- logMessageTimestamp'
  Parsec.spaces
  messageText <- logMessageText'
  eol
  return $ LogMessage messageType messageTimestamp messageText

logMessageType' :: Parsec.GenParser Char st MessageType
logMessageType' = do
  (Parsec.char 'E' >> Parsec.spaces >> errorMessageType')
  <|> (Parsec.char 'I' >> (return Info))
  <|> (Parsec.char 'W' >> (return Warning))

errorMessageType' :: Parsec.GenParser Char st MessageType
errorMessageType' = do
  messageLevel <- integer'
  return (Error messageLevel)

logMessageTimestamp' = integer'

integer' :: Parsec.GenParser Char st TimeStamp
integer' = do
  result <- Parsec.many1 Parsec.digit
  return (foldl' (\a i -> a * 10 + digitToInt i) 0 result)

logMessageText' :: Parsec.GenParser Char st String
logMessageText' = Parsec.many $ Parsec.noneOf "\n"

insert :: LogMessage -> MessageTree -> MessageTree
insert new Leaf =
  Node Leaf new Leaf
insert new@(LogMessage _ newTimestamp _) (Node left cur@(LogMessage _ curTimestamp _) right) =
  if (newTimestamp < curTimestamp) then
    Node (insert new left) cur right
  else
    Node left cur (insert new right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build msgs =
  foldl' (flip insert) Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) =
  inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  let
    atLeastError50 (LogMessage (Error n) _ _)  = n >= 50
    atLeastError50 (anything) = False

    logText (LogMessage _ _ text) = text
  in
    map logText . inOrder . build . filter atLeastError50
