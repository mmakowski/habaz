module CLIP(
  CLIPMessage (..), 
  ParseResult (Success, Failure),
  PlayerInfo (..),
  RedoubleLimit (..),
  Flag (..),
  Command (..),
  parseCLIPMessage,
  parseCLIPMessages,
  formatCommand
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
     | FreeForm String
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
     | Login { name :: String
             , message :: String
             }
     | Logout { name :: String
              , message :: String
              }
     | Message { from :: String
               , time :: UTCTime
               , message :: String
               }
     | MessageDelivered { name :: String }
     | MessageSaved { name :: String }
     | Says { name :: String
            , message :: String
            }
     | Shouts { name :: String
              , message :: String
              }
     | Whispers { name :: String
                , message :: String
                }
     | Kibitzes { name :: String
                , message :: String
                }
     | YouSay { name :: String
              , message :: String
              }
     | YouShout { message :: String }
     | YouWhisper { message :: String }
     | YouKibitz { message :: String }
     deriving (Eq, Show)

data Flag
     = Ready
     deriving (Eq, Show)

data Command
     = Toggle Flag
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

parseCLIPMessages :: String -> [ParseResult CLIPMessage]
parseCLIPMessages str = 
  let (msg, rest) = parseCLIPMessage str 
  in (msg : parseCLIPMessages rest)

parseCLIPMessage :: String -> (ParseResult CLIPMessage, String)
parseCLIPMessage str = 
  let (first, rest) = firstLineAndRest str 
      (msgNum, restOfLine) = msgNumAndRest first
  in parseLine msgNum (stripCRLF restOfLine) rest

parseLine :: Maybe Int -> String -> String -> (ParseResult CLIPMessage, String)
parseLine Nothing = parseUnprefixedLine
parseLine (Just n) = case n of
  1  -> parseWelcome
  2  -> parseOwnInfo
  3  -> parseMOTD
  5  -> parseWhoInfo
  7  -> parseLogin
  8  -> parseLogout
  9  -> parseMessage
  10 -> parseMessageDelivered
  11 -> parseMessageSaved
  12 -> parseSays
  13 -> parseShouts
  14 -> parseWhispers
  15 -> parseKibitzes
  --16 -> parseYouSay
  _ -> \line rest -> (Failure $ "unrecognised message type id " ++ (show n) ++ "; rest of line: '" ++ line ++"'", 
                      rest)

parseUnprefixedLine "login:" rest = (Success FailedLogin, rest)
parseUnprefixedLine "" rest = parseFreeForm rest
parseUnprefixedLine line rest = (Failure $ "unable to parse line: '" ++ line ++ "'", rest)

parseWelcome line rest =
  let [name, lastLogin, lastHost] = words line
  in (Welcome <$> pure name 
              <*> parseUTCTime lastLogin 
              <*> pure lastHost, 
      rest)

parseOwnInfo line rest =
  let [name, allowpip, autoboard, autodouble, automove, away, bell, crawford, 
       double, experience, greedy, moreboards, moves, notify, rating, ratings, ready, 
       redoubles, report, silent, timezone] = words line
  in (OwnInfo <$> pure name 
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

parseMOTD _ rest =
  let (motd, rest') = readMOTD "" rest
  in (Success $ MOTD motd, rest')
  where
    readMOTD acc str = 
      let (first, rest) = firstLineAndRest str
      in case msgNumAndRest first of 
        ((Just 4), _) -> (acc, rest)
        _ -> readMOTD (acc ++ first) rest

parseWhoInfo line rest =
  let (infos, rest') = parsePlayerInfos [] (Just 5) line rest
  in (WhoInfo <$> lift infos, rest')
  where 
    parsePlayerInfos acc (Just 6) _ rest = (acc, rest) 
    parsePlayerInfos acc msgNum line rest = 
      let acc' = acc ++ [parsePlayerInfo $ words line]
          (next, rest') = firstLineAndRest rest
          (msgNum, line') = msgNumAndRest next
      in parsePlayerInfos acc' msgNum line' rest'
    lift [] = pure []
    lift (pr:prs) = (:) <$> pr <*> lift prs
  
parseLogin = parseGenericNameMsg Login

parseLogout = parseGenericNameMsg Logout

parseMessage line rest =
  let (from, line') = firstWordAndRest line
      (time, msg) = firstWordAndRest line'
  in (Message <$> pure from
              <*> parseUTCTime time
              <*> pure msg,
      rest)

parseMessageDelivered name rest = 
  (Success (MessageDelivered name), rest)

parseMessageSaved name rest =
  (Success (MessageSaved name), rest)

parseSays = parseGenericNameMsg Says

parseShouts = parseGenericNameMsg Shouts

parseWhispers = parseGenericNameMsg Whispers

parseKibitzes = parseGenericNameMsg Kibitzes

parseGenericNameMsg cons line rest =  
  let (name, msg) = firstWordAndRest line
  in (Success (cons name msg), rest)
     
parseFreeForm :: String -> (ParseResult CLIPMessage, String)
parseFreeForm str = 
  let (first, rest) = firstLineAndRest str
  in (Success (FreeForm (stripCRLF first)), rest)

parsePlayerInfo :: [String] -> ParseResult PlayerInfo
parsePlayerInfo [name, opponent, watching, ready, away, rating, experience, idle, 
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

-- command formatting
  
formatCommand :: Command -> String
formatCommand (Toggle flag) = "toggle " ++ (formatFlag flag)

formatFlag :: Flag -> String
formatFlag flag = case flag of 
  Ready -> "ready"


-- helper string splitting functions

lineTerminators = ["\r\n", "login:"]

firstLineAndRest :: String -> (String, String)
firstLineAndRest = firstAndRest lineTerminators True

firstWordAndRest :: String -> (String, String)
firstWordAndRest = firstAndRest [" ", "\r\n"] False

msgNumAndRest :: String -> (Maybe Int, String)
msgNumAndRest str = 
  let (first, rest) = firstWordAndRest str
  in case reads first of
    [(num, "")] -> (Just num, rest)
    _ -> (Nothing, str)

firstAndRest :: [String] -> Bool -> String -> (String, String)
firstAndRest terminators retainTerm str = 
  let (revFirst, rest) = loop [] str in (reverse revFirst, rest)
  where
    rTermStrs = map reverse terminators
    loop acc [] = (acc, [])
    loop acc (h:t) = 
      let acc' = h:acc
      in case findPrefix rTermStrs acc' of  
        Just p -> (if retainTerm then acc' else drop (length p) acc', t)
        Nothing -> loop acc' t

findPrefix :: [String] -> String -> Maybe String
findPrefix [] str = Nothing
findPrefix (prefix:prefixes) str = 
  if prefix `isPrefixOf` str then (Just prefix) else findPrefix prefixes str

stripCRLF :: String -> String
stripCRLF str = if "\r\n" `isSuffixOf` str then init (init str) else str

