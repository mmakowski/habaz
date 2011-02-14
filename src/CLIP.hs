module CLIP where
import Control.Applicative
import Data.List
import Data.Time
import System.Locale

data CLIPMessage 
                 = ParseFailure String -- ^ error message
                 | FailedLogin
                 -- | Welcome: username, last login, last host
                 | Welcome String UTCTime String
                 -- | OwnInfo: username, allowpip, autoboard, autodouble, automove
                 | OwnInfo String     -- username
                           Bool       -- allowpip
                           Bool       -- autoboard
                           Bool       -- autodouble
                           Bool       -- automove
                   deriving (Eq, Show)


-- message parsing

lineTerminators = ["\n", "login:"]

parseMessage str = 
  let (first, rest) = getFirstLine str 
  in parseLine (words first) rest
  where
    getFirstLine str = 
      let (revFirst, rest) = loop [] str in (reverse revFirst, rest)
      where
        rTermStrs = map reverse lineTerminators
        isTerminated rstr (rTermStr:rTermStrs) = isPrefixOf rTermStr rstr || isTerminated rstr rTermStrs
        isTerminated _ [] = False
        loop acc [] = (acc, [])
        loop acc (h:t) = 
          let currStr = (h:acc) 
          in if isTerminated currStr rTermStrs then (currStr, t) else loop currStr t
    parseUTCTime = parseTime defaultTimeLocale "%s"
    parseBool "1" = Just True
    parseBool "0" = Just False
    parseBool _ = Nothing
    -- failed login
    parseLine ("login:":_) rest = (FailedLogin, rest)
    -- CLIP Welcome
    parseLine ["1", username, millisString, ip] rest = case parseUTCTime millisString of
      Just time -> (Welcome username time ip, rest)
      Nothing -> (ParseFailure $ "unable to parse " ++ millisString ++ " as time", rest)
    -- CLIP Own Info
    parseLine w@["2", name, allowpip, autoboard, autodouble, automove, away, bell, crawford, 
               double, experience, greedy, moreboards, moves, notify, rating, ratings, ready, 
               redoubles, report, silent, timezone] rest = 
      case OwnInfo <$> Just name <*> 
                       parseBool allowpip <*> 
                       parseBool autoboard <*> 
                       parseBool autodouble <*>
                       parseBool automove of
        Just ownInfo -> (ownInfo, rest)
        Nothing -> (ParseFailure $ "unable to parse " ++ (unwords w) ++ " as own info", rest)
    -- TODO: other cases
    parseLine words rest = (ParseFailure $ "unable to parse: " ++ (unwords words), rest)

parseMessages str = 
  let (msg, rest) = parseMessage str in (msg : parseMessages rest)

