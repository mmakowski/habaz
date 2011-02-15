module CLIP(
  CLIPMessage (..), 
  ParseResult (Success, Failure),
  PlayerInfo (..),
  RedoubleLimit (..),
  parseMessage,
  parseMessages
  ) where
import Control.Applicative
import Data.List
import Data.Time
import System.Locale

-- TODO: move to a separate Backgammon module
data RedoubleLimit
     = LimitedTo Int
     | Unlimited
     deriving (Eq, Show)

data PlayerInfo 
     = PlayerInfo { pName :: String
                  , pOpponent :: Maybe String
                  , pWatching :: Maybe String
                  , pReady :: Bool
                  , pAway :: Bool
                  , pRating :: Float
                  , pExperience :: Int
                  , pIdle :: Int
                  , pLogin :: UTCTime
                  , pHostName :: String
                  , pClient :: Maybe String
                  , pEmail :: Maybe String
                  }
     deriving (Eq, Show)

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
               , away :: Bool
               , bell :: Bool
               , crawford :: Bool
               , double :: Bool
               , experience :: Int
               , greedy :: Bool
               , moreBoards :: Bool
               , moves :: Bool
               , notify :: Bool
               , rating :: Float
               , ratings :: Bool
               , ready :: Bool
               , redoubles :: RedoubleLimit
               , report :: Bool
               , silent :: Bool
               , timeZone :: String
               }
     | MOTD String
     | WhoInfo [PlayerInfo]
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
  let (first, rest) = firstLineAndRest str 
  in parseLine (words first) rest

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
  (OwnInfo <$> pure name 
           <*> parseBool allowpip 
           <*> parseBool autoboard 
           <*> parseBool autodouble
           <*> parseBool automove 
           <*> parseBool away
           <*> parseBool bell
           <*> parseBool crawford
           <*> parseBool double
           <*> parse experience
           <*> parseBool greedy
           <*> parseBool moreboards
           <*> parseBool moves
           <*> parseBool notify
           <*> parse rating
           <*> parseBool ratings
           <*> parseBool ready
           <*> parseRedoubleLimit redoubles
           <*> parseBool report
           <*> parseBool silent
           <*> pure timezone,
   rest)
-- CLIP MOTD
parseLine ["3"] rest = 
  let (motd, rest') = readMOTD "" rest
  in (Success $ MOTD motd, rest')
  where
    readMOTD acc str = 
      let (first, rest) = firstLineAndRest str
      in case words first of 
        ["4"] -> (acc, rest)
        _ -> readMOTD (acc ++ first) rest
-- CLIP Who Info
parseLine line@("5" : _) rest =
  let (infos, rest') = parseWhoInfo [] line rest
  in (WhoInfo <$> lift infos, rest')
  where 
    parseWhoInfo acc ["6"] rest = (acc, rest)
    parseWhoInfo acc line@("5" : _) rest = 
      let (next, rest') = firstLineAndRest rest
      in parseWhoInfo (acc ++ [parsePlayerInfo line]) (words next) rest'
    lift [] = pure []
    lift (pr:prs) = (:) <$> pr <*> lift prs
-- TODO: other cases
parseLine words rest = (Failure $ "unable to parse: " ++ (unwords words), rest)

parsePlayerInfo ["5", name, opponent, watching, ready, away, rating, experience, idle, 
                 login, hostname, client, email] = 
  PlayerInfo <$> pure name
             <*> parseMaybeString opponent
             <*> parseMaybeString watching
             <*> parseBool ready
             <*> parseBool away
             <*> parse rating
             <*> parse experience
             <*> parse idle
             <*> parseUTCTime login
             <*> pure hostname
             <*> parseMaybeString client
             <*> parseMaybeString email
parsePlayerInfo w = Failure $ "unable to parse " ++ (show w) ++ " as player info"

firstLineAndRest :: String -> (String, String)
firstLineAndRest str = 
  let (revFirst, rest) = loop [] str in (reverse revFirst, rest)
  where
    rTermStrs = map reverse lineTerminators
    loop acc [] = (acc, [])
    loop acc (h:t) = 
      let currStr = (h:acc) 
      in if rTermStrs `containsPrefixOf` currStr then (currStr, t) else loop currStr t

strs `containsPrefixOf` str = foldl (\b s -> b || s `isPrefixOf` str) False strs

lineTerminators = ["\n", "login:"]

parse :: Read a => String -> ParseResult a
parse str = case reads str of
  [(val, "")] -> Success val
  _ -> Failure $ "unable to parse '" ++ str ++ "'"

parseBool :: String -> ParseResult Bool
parseBool "1" = Success True
parseBool "0" = Success False
parseBool str = Failure $ "unable to parse '" ++ str ++ "' as Bool"

parseMaybeString :: String -> ParseResult (Maybe String)
parseMaybeString "-" = Success Nothing
parseMaybeString str = Success (Just str)

parseRedoubleLimit :: String -> ParseResult RedoubleLimit
parseRedoubleLimit "unlimited" = Success Unlimited
parseRedoubleLimit str = case reads str of
  [(num, "")] -> Success $ LimitedTo num
  _ -> Failure $ "unable to parse '" ++ str ++"' as redoubles limit"

parseUTCTime :: String -> ParseResult UTCTime
parseUTCTime str = case parseTime defaultTimeLocale "%s" str of
  Just time -> Success time
  Nothing -> Failure $ "unable to parse '" ++ str ++ "' as UTCTime"

