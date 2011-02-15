module CLIP(
  CLIPMessage (..), 
  ParseResult (Success, Failure),
  parseMessage,
  parseMessages
  ) where
import Control.Applicative
import Data.List
import Data.Time
import System.Locale

data CLIPMessage 
     = FailedLogin
     | Welcome { name :: String
               , lastLogin :: UTCTime
               , lastHost :: String 
               }
     | OwnInfo { name :: String 
               , allowPip :: Bool 
               , autoBoard :: Bool 
               , autoDouble :: Bool 
               , autoMove :: Bool       
               }
     deriving (Eq, Show)

data ParseResult a 
     = Success a
     | Failure String
     deriving (Eq, Show)

instance Functor ParseResult where 
  fmap f (Failure msg) = Failure msg
  fmap f (Success a) = Success (f a)
    
instance Applicative ParseResult where
  pure = Success
  Failure msg1 <*> Failure msg2 = Failure $ msg1 ++ "; " ++ msg2
  Failure msg <*> _ = Failure msg
  Success f <*> sth = fmap f sth
  
-- message parsing

parseMessages :: String -> [ParseResult CLIPMessage]
parseMessages str = 
  let (msg, rest) = parseMessage str 
  in (msg : parseMessages rest)

parseMessage :: String -> (ParseResult CLIPMessage, String)
parseMessage str = 
  let (first, rest) = firstLine str 
  in parseLine (words first) rest
  where
    firstLine str = 
      let (revFirst, rest) = loop [] str in (reverse revFirst, rest)
    rTermStrs = map reverse lineTerminators
    loop acc [] = (acc, [])
    loop acc (h:t) = 
      let currStr = (h:acc) 
      in if rTermStrs `containsPrefixOf` currStr then (currStr, t) else loop currStr t

parseLine :: [String] -> String -> (ParseResult CLIPMessage, String)
-- failed login
parseLine ("login:" : _) rest = (Success FailedLogin, rest)
-- CLIP Welcome
parseLine ["1", username, millisString, ip] rest = 
  (Welcome <$> pure username <*> parseUTCTime millisString <*> pure ip, rest)
-- CLIP Own Info
parseLine ["2", name, allowpip, autoboard, autodouble, automove, away, bell, crawford, 
           double, experience, greedy, moreboards, moves, notify, rating, ratings, ready, 
           redoubles, report, silent, timezone] rest = 
  (OwnInfo <$> pure name <*> 
               parseBool allowpip <*> 
               parseBool autoboard <*> 
               parseBool autodouble <*>
               parseBool automove,
   rest)
-- TODO: other cases
parseLine words rest = (Failure $ "unable to parse: " ++ (unwords words), rest)

strs `containsPrefixOf` str = foldl (\b s -> b || s `isPrefixOf` str) False strs

lineTerminators = ["\n", "login:"]

parseUTCTime :: String -> ParseResult UTCTime
parseUTCTime str = case parseTime defaultTimeLocale "%s" str of
  Just time -> Success time
  Nothing -> Failure $ "unable to parse '" ++ str ++ "' as UTCTime"

parseBool :: String -> ParseResult Bool
parseBool "1" = Success True
parseBool "0" = Success False
parseBool str = Failure $ "unable to parse '" ++ str ++ "' as Bool"

