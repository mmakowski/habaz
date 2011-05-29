{-|
This module contains data types representing messages received from FIBS and functions for working
with them. The message representation is based on <http://www.fibs.com/fibs_interface.html> with some minor 
changes to make it more practical to represent the stream of messages as a list.
-}
module FIBSClient.Messages( 
  -- * Types
  FIBSMessage(..),
  ParseResult(..),
  RedoubleLimit (..),
  -- * Functions
  -- ** Parsing
  parseFIBSMessage,
  parseFIBSMessages,
  -- ** Message predicates
  isFreeForm,
  isSystem,
  isReadyOn,
  isOwnInfo,
  isWhoInfo,
  isTerminating,
  -- ** Message list operations
  splitByFirst,
  splitByFirstWithLimit
) where
import Control.Applicative
import Data.List
import Data.Time
import Test.HUnit
import Test.QuickCheck
import TestUtils
import System.Locale

data RedoubleLimit
     = LimitedTo Int
     | Unlimited
     deriving (Eq, Show)

data FIBSMessage 
     = LoginPrompt
     | FreeForm String
       -- | System messages are ones prefixed with two asterisks, except for special cases which are
       -- interpreted further, like 'ReadyOn' etc.
     | System { message :: String }
     | ReadyOn
     | ReadyOff
     | ConnectionTimeOut
     | EndOfGoodbyeMessage
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
     -- | Note that this represents a single line of WhoInfo as opposed to the entire block, like
     -- in <http://www.fibs.com/fibs_interface.html>
     | WhoInfo { name :: String
               , opponent :: Maybe String
               , watching :: Maybe String
               , ready :: Bool
               , away :: Bool
               , rating :: Float
               , experience :: Int
               , idle :: Int
               , loginTime :: UTCTime
               , hostName :: String
               , client :: Maybe String
               , email :: Maybe String
               }
     | EndOfWhoInfoBlock
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

data ParseResult a 
     = ParseSuccess a
     | ParseFailure String
     deriving (Eq, Show)

instance Functor ParseResult where 
  fmap f (ParseFailure msg) = ParseFailure msg
  fmap f (ParseSuccess a) = ParseSuccess (f a)
    
instance Applicative ParseResult where
  pure = ParseSuccess
  ParseFailure msg1 <*> ParseFailure msg2 = ParseFailure $ msg1 ++ "; " ++ msg2
  ParseFailure msg <*> _ = ParseFailure msg
  ParseSuccess f <*> sth = fmap f sth

instance Arbitrary a => Arbitrary (ParseResult a) where
  arbitrary = do a <- arbitrary
                 msg <- arbitrary
                 elements [ParseSuccess a, ParseFailure msg]

-- interface functions

-- | When applied to a string containing FIBS output yields a (lazy) list of parsed messages.
parseFIBSMessages :: String  -- ^ the string to parse, typically this will be read from socket connected to FIBS
                  -> [ParseResult FIBSMessage] -- ^ a list of messages (or parse failures)
parseFIBSMessages str = 
  let (msg, rest) = parseFIBSMessage str 
  in (msg : parseFIBSMessages rest)

test_parseFIBSMessagesIsLazy = 
  assertEqual "parseFIBSMessages is lazy" 
              [ParseSuccess LoginPrompt, ParseSuccess LoginPrompt]
              (take 2 $ parseFIBSMessages $ cycle "login:")

-- | Parses a single message from given string.
parseFIBSMessage :: String                            -- ^ the string to parse
                 -> (ParseResult FIBSMessage, String) -- ^ the result of parsing and unconsumed input
parseFIBSMessage str = 
  let (first, rest) = firstLineAndRest str 
      (msgNum, restOfLine) = msgNumAndRest first
  in parseLine msgNum (stripCRLF restOfLine) rest

-- | Splits the list of ParseResults by the first ParseSuccess whose content matches the predicate given.
-- If no elements match the predicate an error is raised.
splitByFirst :: [ParseResult a]                       -- ^ the list to split
             -> (a -> Bool)                           -- ^ the predicate to test parsed elements
             -> ([ParseResult a], a, [ParseResult a]) -- ^ results before the first matching element, 
                                                      -- first matching element, the rest of the list
m `splitByFirst` c = splitByFirst' m c [] where
  splitByFirst' (m@(ParseFailure _):msgs) cond dropped = splitByFirst' msgs cond (m:dropped)
  splitByFirst' (m@(ParseSuccess msg):msgs) cond dropped = 
    if cond msg then (reverse dropped, msg, msgs) else splitByFirst' msgs cond (m:dropped)

test_splitByFirstPreservesFailures = 
  assertEqual ""
              ([ParseFailure "0", ParseSuccess 1], 2, [ParseFailure "3"])
              ([ParseFailure "0", ParseSuccess 1, ParseSuccess 2, ParseFailure "3"] `splitByFirst` (== 2))

-- | Like 'splitByFirst', but with a limit on how many entries to check for an entry matching
-- the predicate before giving up. Returns Nothing if a matching entry has not been found within
-- the first n entries.
splitByFirstWithLimit :: [ParseResult a]  -- ^ the list to split
                      -> (a -> Bool)      -- ^ the predicate to test elements
                      -> Int              -- ^ the limit of elements to check
                      -> Maybe ([ParseResult a], a, [ParseResult a]) -- ^ 'Just' result, as in 'splitByFirst', or
                                                                     -- 'Nothing' if no matching entry found 
                                                                     -- within limit
splitByFirstWithLimit m c limit = splitByFirstWithLimit' m c limit [] where
  splitByFirstWithLimit' _ _ 0 _ = Nothing
  splitByFirstWithLimit' (m@(ParseFailure _):msgs) cond limit dropped = 
    splitByFirstWithLimit' msgs cond (limit - 1) (m:dropped)
  splitByFirstWithLimit' (m@(ParseSuccess msg):msgs) cond limit dropped =     
    if cond msg then Just (reverse dropped, msg, msgs) 
    else splitByFirstWithLimit' msgs cond (limit - 1) (m:dropped)
    
test_splitByFirstWithLimitStopsAtLimit =
  assertEqual "" Nothing (splitByFirstWithLimit [ps 1, ps 2, ps 3, ps 4] (== 3) 2)
  where ps = ParseSuccess

test_splitByFirstWithLimitFindsElementAtLimit =
  assertEqual "" 
              (Just ([ps 1, ps 2], 3, [ps 4]))
              (splitByFirstWithLimit [ps 1, ps 2, ps 3, ps 4] (== 3) 3)
  where ps = ParseSuccess
        
-- message predicates  

isFreeForm (FreeForm _) = True
isFreeForm _ = False
isSystem (System _) = True
isSystem _ = False
isReadyOn ReadyOn = True
isReadyOn _ = False
isOwnInfo (OwnInfo _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isOwnInfo _ = False
isWhoInfo (WhoInfo _ _ _ _ _ _ _ _ _ _ _ _) = True
isWhoInfo _ = False
-- TODO: are the others needed? They have different type.
isTerminating (ParseSuccess ConnectionTimeOut) = True
isTerminating (ParseSuccess EndOfGoodbyeMessage) = True
isTerminating _ = False

-- message parsing

parseLine :: Maybe Int -> String -> String -> (ParseResult FIBSMessage, String)
parseLine Nothing = parseUnprefixedLine
parseLine (Just n) = case n of
  1  -> parseWelcome
  2  -> parseOwnInfo
  3  -> parseMOTD
  5  -> parseWhoInfo
  6  -> skip
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
  _ -> \line rest -> (ParseFailure $ "unrecognised message type id " ++ (show n) ++ "; rest of line: '" ++ line ++"'", 
                      rest)

-- failed login
parseUnprefixedLine "login:" rest = (ParseSuccess LoginPrompt, rest)
-- system message
parseUnprefixedLine ('*':('*':(' ':msg))) rest = (ParseSuccess (recognise msg), rest)
  where 
    recognise "You're now ready to invite or join someone." = ReadyOn
    recognise "You're now refusing to play with someone." = ReadyOff
    recognise msg = System msg
-- empty line
parseUnprefixedLine "" rest = skip "" rest
-- free form
parseUnprefixedLine line rest = (ParseSuccess (recognise line), rest)
  where
    recognise "Connection timed out." = ConnectionTimeOut
    recognise "                      Keep them coming...." = EndOfGoodbyeMessage
    recognise line = FreeForm line

test_loginPromptParsedCorrectly = 
  assertEqual "login prompt" 
              (ParseSuccess LoginPrompt, []) 
              (parseFIBSMessage "login:")

test_systemParsedCorrectly =
  "** Some system message.\r\n" `parsesTo` (System "Some system message.")

test_readyOnParsedCorrectly =
  "** You're now ready to invite or join someone.\r\n" `parsesTo` ReadyOn
  
test_readyOffParsedCorrectly = 
  "** You're now refusing to play with someone.\r\n" `parsesTo` ReadyOff
  
test_blankLineIsSkipped = 
  assertEqual "blank line is skipped"
              (ParseSuccess (System "system message"), [])
              (parseFIBSMessage "\r\n** system message")

test_connectionTimeOutParsedCorrectly =
  "Connection timed out." `parsesTo` ConnectionTimeOut
  
test_endOfGoodbyeMessageParsedCorrectly =
  "                      Keep them coming....\r\n" `parsesTo` EndOfGoodbyeMessage

test_freeFormParsedCorrectly =
  assertEqual "free form"
              (ParseSuccess (FreeForm "aleks and Ubaretzu start a 1 point match."), [])
              (parseFIBSMessage "\r\naleks and Ubaretzu start a 1 point match.\r\n")
              

skip _ rest = parseFIBSMessage rest

-- Welcome
parseWelcome line rest =
  let [name, lastLogin, lastHost] = words line
  in (Welcome <$> pure name 
              <*> parseUTCTime lastLogin 
              <*> pure lastHost, 
      rest)

test_welcomeParsedCorrectly = 
  "1 username 1041253132 1.2.3.4\r\n" `parsesTo` (Welcome "username" (toUTCTime "1041253132") "1.2.3.4")

-- OwnInfo
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

test_ownInfoParsedCorrectly = 
  "2 myself 1 1 0 0 0 0 1 1 2396 0 1 0 1 3457.85 0 0 0 0 0 Australia/Melbourne\r\n" `parsesTo`
  (OwnInfo "myself" True True False False False False True True 2396 False True False True 3457.85 False False (LimitedTo 0) False False "Australia/Melbourne")

-- MOTD
parseMOTD _ rest =
  let (motd, rest') = readMOTD "" rest
  in (ParseSuccess $ MOTD motd, rest')
  where
    readMOTD acc str = 
      let (first, rest) = firstLineAndRest str
      in case msgNumAndRest first of 
        ((Just 4), _) -> let (_, rest') = firstLineAndRest rest in (acc, rest')
        _ -> readMOTD (acc ++ first) rest

test_motdParsedCorrectly = 
  ("3\r\n" ++ motd ++ "4\r\n\r\n") `parsesTo` (MOTD motd)
  where 
    motd = 
      "+--------------------------------------------------------------------+\r\n\
      \|                                                                    |\r\n\
      \| It was a dark and stormy night in Oakland.  Outside, the rain      |\r\n\
      \| came down in torrents.  Winds of 40MPH+ pounded at the windows,    |\r\n\
      \| and whipped at trees, power lines, and anyone foolish enough       |\r\n\
      \| to be outdoors.                                                    |\r\n\
      \|                                                                    |\r\n\
      \| In the middle of the night, I was awakened by a loud BEEP BEEP     |\r\n\
      \| BEEP BEEP.  \"What is that?\" I thought groggily.  Ah!  We've had    |\r\n\
      \| a power failure, and the computers are running on battery power.   |\r\n\
      \|                                                                    |\r\n\
      \| Curious to see how things were working, I crawled out of bed and   |\r\n\
      \| stumbled downstairs to log in.  To my delight, people were         |\r\n\
      \| merrily playing backgammon, oblivious to the fact that they had    |\r\n\
      \| just ridden out a power failure that a few months ago would have   |\r\n\
      \| shut down the server most ungracefully.                            |\r\n\
      \|                                                                    |\r\n\
      \| Thanks to all of the generous FIBSters who bought the UPS and      |\r\n\
      \| made this possible!  And coming soon, as soon as I can get it      |\r\n\
      \| built and deployed, a new (more reliable, and maybe faster)        |\r\n\
      \| server.                                                            |\r\n\
      \|                                                                    |\r\n\
      \+--------------------------------------------------------------------+\r\n"

-- WhoInfo
parseWhoInfo line rest = parseWhoInfoWords (words line) rest
parseWhoInfoWords [name, opponent, watching, ready, away, rating, experience, idle, 
                   login, hostname, client, email] rest = 
  (WhoInfo <$> pure name
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
           <*> parseMaybeString email,
   rest)
parseWhoInfoWords w rest = (ParseFailure $ "unable to parse " ++ (show w) ++ " as WhoInfo", rest)

test_whoInfoParsedCorrectly = 
  "5 mgnu_advanced someplayer - 1 0 1912.15 827 8 1040515752 192.168.143.5 3DFiBs -" `parsesTo`
  (WhoInfo "mgnu_advanced" (Just "someplayer") Nothing True False 1912.15 827 8 (toUTCTime "1040515752") "192.168.143.5" (Just "3DFiBs") Nothing)

test_whoInfoParseFailure = 
  ("5 " ++ badLine) `failsToParseWithErr` ("unable to parse " ++ (show $ words badLine) ++ " as WhoInfo")
  where badLine = "mgnu_advanced someplayer - 1 0 1912.15 lubudubu logs in."

test_endOfWhoInfoBlockIsSkipped =
  "6\r\n\r\nsome message\r\n" `parsesTo` (FreeForm "some message")

-- Login
parseLogin = parseGenericNameMsg Login

test_loginParsedCorrectly =
  "7 someplayer someplayer logs in.\r\n" `parsesTo` (Login "someplayer" "someplayer logs in.")

-- Logout
parseLogout = parseGenericNameMsg Logout

test_logoutParsedCorrectly = 
  "8 someplayer someplayer drops connection.\r\n" `parsesTo` (Logout "someplayer" "someplayer drops connection.")

-- Message
parseMessage line rest =
  let (from, line') = firstWordAndRest line
      (time, msg) = firstWordAndRest line'
  in (Message <$> pure from
              <*> parseUTCTime time
              <*> pure msg,
      rest)

test_messageParsedCorrectly = 
  "9 someplayer 1041253132 I'll log in at 10pm if you want to finish that game.\r\n" `parsesTo` 
  (Message "someplayer" (toUTCTime "1041253132") "I'll log in at 10pm if you want to finish that game.")

-- MessageDelivered
parseMessageDelivered name rest = 
  (ParseSuccess (MessageDelivered name), rest)

test_messageDeliveredParsedCorrectly = 
  "10 someplayer\r\n" `parsesTo` (MessageDelivered "someplayer")
              
-- MessageSaved
parseMessageSaved name rest =
  (ParseSuccess (MessageSaved name), rest)

test_messageSavedParsedCorrectly = 
  "11 someplayer\r\n" `parsesTo` (MessageSaved "someplayer")
              
-- Says
parseSays = parseGenericNameMsg Says

test_saysParsedCorrectly = 
  "12 someplayer Do you want to play a game?\r\n" `parsesTo` (Says "someplayer" "Do you want to play a game?")

-- Shouts
parseShouts = parseGenericNameMsg Shouts

test_shoutsParsedCorrectly = 
  "13 someplayer Anybody for a 5 point match?\r\n" `parsesTo` (Shouts "someplayer" "Anybody for a 5 point match?")

-- Whispers
parseWhispers = parseGenericNameMsg Whispers

test_whispersParsedCorrectly = 
  "14 someplayer I think he is using loaded dice  :-)\r\n" `parsesTo` 
  (Whispers "someplayer" "I think he is using loaded dice  :-)")

-- Kibitzes
parseKibitzes = parseGenericNameMsg Kibitzes

test_kibitzesParsedCorrectly = 
  "15 someplayer G'Day and good luck from Hobart, Australia.\r\n" `parsesTo` 
  (Kibitzes "someplayer" "G'Day and good luck from Hobart, Australia.")


-- helper parsing functions

parseGenericNameMsg cons line rest =  
  let (name, msg) = firstWordAndRest line
  in (ParseSuccess (cons name msg), rest)
     
parse :: Read a => String -> ParseResult a
parse str = case reads str of
  [(val, "")] -> ParseSuccess val
  _ -> ParseFailure $ "unable to parse '" ++ str ++ "'"

parseBool :: String -> ParseResult Bool
parseBool "1" = ParseSuccess True
parseBool "0" = ParseSuccess False
parseBool str = ParseFailure $ "unable to parse '" ++ str ++ "' as Bool"

parseMaybeString :: String -> ParseResult (Maybe String)
parseMaybeString "-" = ParseSuccess Nothing
parseMaybeString str = ParseSuccess (Just str)

parseRedoubleLimit :: String -> ParseResult RedoubleLimit
parseRedoubleLimit "unlimited" = ParseSuccess Unlimited
parseRedoubleLimit str = case reads str of
  [(num, "")] -> ParseSuccess $ LimitedTo num
  _ -> ParseFailure $ "unable to parse '" ++ str ++"' as redoubles limit"

parseUTCTime :: String -> ParseResult UTCTime
parseUTCTime str = case parseTime defaultTimeLocale "%s" str of
  Just time -> ParseSuccess time
  Nothing -> ParseFailure $ "unable to parse '" ++ str ++ "' as UTCTime"


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

-- helper test functions

str `parsesTo` msg = assertEqual "" (ParseSuccess msg, []) (parseFIBSMessage str)
str `failsToParseWithErr` err = assertEqual "" (ParseFailure err, []) (parseFIBSMessage str)
