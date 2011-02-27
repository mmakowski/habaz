module FIBSClient where -- TODO: limit exports
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Network.Socket hiding (connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO
import System.Locale
import Test.HUnit
import Test.QuickCheck


type Connection = Handle
                  
data LoginStatus = LoginSuccess
                 | LoginFailure String
                   
data RedoubleLimit
     = LimitedTo Int
     | Unlimited
     deriving (Eq, Show)

data FIBSMessage 
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
     | System { message :: String }
     deriving (Eq, Show)

data Flag
     = Ready
     deriving (Eq, Show)

data Command
     = Toggle Flag
     deriving (Eq, Show)

instance Test.QuickCheck.Arbitrary Command where
  arbitrary = elements [Toggle Ready] -- TODO: more examples
  coarbitrary _ = id -- TODO

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


-- message parsing

parseFIBSMessages :: String -> [ParseResult FIBSMessage]
parseFIBSMessages str = 
  let (msg, rest) = parseFIBSMessage str 
  in (msg : parseFIBSMessages rest)

test_parseFIBSMessagesIsLazy = 
  assertEqual "parseFIBSMessages is lazy" 
              [ParseSuccess FailedLogin, ParseSuccess FailedLogin]
              (take 2 $ parseFIBSMessages $ cycle "login:")

parseFIBSMessage :: String -> (ParseResult FIBSMessage, String)
parseFIBSMessage str = 
  let (first, rest) = firstLineAndRest str 
      (msgNum, restOfLine) = msgNumAndRest first
  in parseLine msgNum (stripCRLF restOfLine) rest

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
parseUnprefixedLine "login:" rest = (ParseSuccess FailedLogin, rest)
-- system message
parseUnprefixedLine ('*':('*':(' ':msg))) rest = (ParseSuccess (System msg), rest)
-- empty line
parseUnprefixedLine "" rest = skip "" rest
-- free form
parseUnprefixedLine line rest = (ParseSuccess (FreeForm line), rest)

test_failedLoginParsedCorrectly = 
  assertEqual "failed login" 
              (ParseSuccess FailedLogin, []) 
              (parseFIBSMessage "login:")

test_systemParsedCorrectly =
  assertEqual "System **"
              (ParseSuccess (System "You're now ready to invite or join someone."), [])
              (parseFIBSMessage "** You're now ready to invite or join someone.\r\n")

test_blankLineIsSkipped = 
  assertEqual "blank line is skipped"
              (ParseSuccess (System "system message"), [])
              (parseFIBSMessage "\r\n** system message")

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
  assertEqual "Welcome"
              (ParseSuccess (Welcome "username" (toUTCTime "1041253132") "1.2.3.4"), [])
              (parseFIBSMessage "1 username 1041253132 1.2.3.4\r\n")

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
  assertEqual "Own Info"
              (ParseSuccess (OwnInfo "myself" True True False False False False True True 2396 False True False True 3457.85 False False (LimitedTo 0) False False "Australia/Melbourne"), [])
              (parseFIBSMessage "2 myself 1 1 0 0 0 0 1 1 2396 0 1 0 1 3457.85 0 0 0 0 0 Australia/Melbourne\r\n")

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
  assertEqual "MOTD"
              (ParseSuccess (MOTD motd), [])
              (parseFIBSMessage $ "3\r\n" ++ motd ++ "4\r\n\r\n")
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
parseWhoInfo line rest = 
  let [name, opponent, watching, ready, away, rating, experience, idle, 
       login, hostname, client, email] = words line
  in (WhoInfo <$> pure name
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

test_whoInfoParsedCorrectly = 
  assertEqual "Who Info"
              (ParseSuccess (WhoInfo "mgnu_advanced" (Just "someplayer") Nothing True False 1912.15 827 8 (toUTCTime "1040515752") "192.168.143.5" (Just "3DFiBs") Nothing), [])
              (parseFIBSMessage "5 mgnu_advanced someplayer - 1 0 1912.15 827 8 1040515752 192.168.143.5 3DFiBs -")

test_endOfWhoInfoBlockIsSkipped =
  assertEqual "end of Who Info block is skipped"
              (ParseSuccess (FreeForm "some message"), [])
              (parseFIBSMessage "6\r\n\r\nsome message\r\n")

-- Login
parseLogin = parseGenericNameMsg Login

test_loginParsedCorrectly =
  assertEqual "Login"
              (ParseSuccess (Login "someplayer" "someplayer logs in."), [])
              (parseFIBSMessage "7 someplayer someplayer logs in.\r\n")

-- Logout
parseLogout = parseGenericNameMsg Logout

test_logoutParsedCorrectly = 
  assertEqual "Logout"
              (ParseSuccess (Logout "someplayer" "someplayer drops connection."), [])
              (parseFIBSMessage "8 someplayer someplayer drops connection.\r\n")

-- Message
parseMessage line rest =
  let (from, line') = firstWordAndRest line
      (time, msg) = firstWordAndRest line'
  in (Message <$> pure from
              <*> parseUTCTime time
              <*> pure msg,
      rest)

test_messageParsedCorrectly = 
  assertEqual "Message"
              (ParseSuccess (Message "someplayer" (toUTCTime "1041253132") "I'll log in at 10pm if you want to finish that game."), [])
              (parseFIBSMessage "9 someplayer 1041253132 I'll log in at 10pm if you want to finish that game.\r\n")

-- MessageDelivered
parseMessageDelivered name rest = 
  (ParseSuccess (MessageDelivered name), rest)

test_messageDeliveredParsedCorrectly = 
  assertEqual "Message Delivered"
              (ParseSuccess (MessageDelivered "someplayer"), [])
              (parseFIBSMessage "10 someplayer\r\n")
              
-- MessageSaved
parseMessageSaved name rest =
  (ParseSuccess (MessageSaved name), rest)

test_messageSavedParsedCorrectly = 
  assertEqual "Message Saved"
              (ParseSuccess (MessageSaved "someplayer"), [])
              (parseFIBSMessage "11 someplayer\r\n")
              
-- Says
parseSays = parseGenericNameMsg Says

test_saysParsedCorrectly = 
  assertEqual "Says"
              (ParseSuccess (Says "someplayer" "Do you want to play a game?"), [])
              (parseFIBSMessage "12 someplayer Do you want to play a game?\r\n")

-- Shouts
parseShouts = parseGenericNameMsg Shouts

test_shoutsParsedCorrectly = 
  assertEqual "Shouts"
              (ParseSuccess (Shouts "someplayer" "Anybody for a 5 point match?"), [])
              (parseFIBSMessage "13 someplayer Anybody for a 5 point match?\r\n")

-- Whispers
parseWhispers = parseGenericNameMsg Whispers

test_whispersParsedCorrectly = 
  assertEqual "Whispers"
              (ParseSuccess (Whispers "someplayer" "I think he is using loaded dice  :-)"), [])
              (parseFIBSMessage "14 someplayer I think he is using loaded dice  :-)\r\n")

-- Kibitzes
parseKibitzes = parseGenericNameMsg Kibitzes

test_kibitzesParsedCorrectly = 
  assertEqual "Kibitzes"
              (ParseSuccess (Kibitzes "someplayer" "G'Day and good luck from Hobart, Australia."), [])
              (parseFIBSMessage "15 someplayer G'Day and good luck from Hobart, Australia.\r\n")


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

-- command formatting
  
formatCommand :: Command -> String
formatCommand (Toggle flag) = "toggle " ++ (formatFlag flag)

prop_formatCommandToggleFlag cmd@(Toggle flag) = "toggle " ++ (map toLower $ show flag) == formatCommand cmd

formatFlag :: Flag -> String
formatFlag = map toLower . show

-- communication primitives

readUntil :: [String] -> Connection -> IO String
readUntil termStrs conn = liftM reverse $ loop []
  where
    rTermStrs = map reverse termStrs
    isTerminated rstr (rTermStr:rTermStrs) = isPrefixOf rTermStr rstr || isTerminated rstr rTermStrs
    isTerminated _ [] = False
    loop acc = do
      input <- hGetChar conn
      putStr $ if isTerminated (input:acc) rTermStrs then reverse (input:acc) else ""
      (if isTerminated (input:acc) rTermStrs then return else loop) (input:acc)
      
readLine :: Connection -> IO String
readLine = readUntil ["\n", "login:"] 

send :: Connection -> String -> IO ()
send conn cmd =
  do hPutStrLn conn cmd
     hFlush conn


-- interface functions

connect :: HostName             -- ^ The host to connect to
        -> String               -- ^ Port number
        -> IO Connection        -- ^ The opened connnection
connect hostname port =
  do addrinfos <- withSocketsDo $ getAddrInfo Nothing (Just hostname) (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     setSocketOption sock KeepAlive 1
     Network.Socket.connect sock (addrAddress serveraddr)
     conn <- socketToHandle sock ReadWriteMode
     return $ conn

disconnect :: Connection -> IO ()
disconnect conn = hClose conn

login :: Connection     -- ^ An open connection
      -> String         -- ^ Client name
      -> String         -- ^ User name
      -> String         -- ^ Password
      -> IO LoginStatus -- ^ The status of loggin attempt
login conn clientname username password = 
  do readUntil ["login:"] conn
     send conn $ "login " ++ clientname ++ " " ++ clipVersion ++ " " ++ username ++ " " ++ password
     readUntil ["\n"] conn
     line <- readUntil ["\n", "login:"] conn
     return $ case fst (parseFIBSMessage line) of
       ParseFailure msg             -> LoginFailure $ "parse error: " ++ msg
       ParseSuccess FailedLogin     -> LoginFailure "invalid login details"
       ParseSuccess (Welcome _ _ _) -> LoginSuccess
     
logout :: Connection    -- ^ An open connection
       -> IO ()
logout conn = send conn $ "bye"
    

readMessages :: Connection -> IO [ParseResult FIBSMessage]
readMessages conn = 
  do msgStr <- hGetContents conn -- lazy!
     return $ parseFIBSMessages msgStr

sendCommand :: Connection -> Command -> IO ()
sendCommand conn cmd = 
  do send conn $ formatCommand cmd
     return ()


-- experiments in progress:

defaultFibsHost = "fibs.com"
defaultFibsPort = "4321"
clipVersion = "1008"

test = 
  do conn <- connect defaultFibsHost defaultFibsPort
     login conn "Habaź" "habaztest_a" "habaztest"
     take 30 <$> readMessages conn >>= mapM_ (putStrLn . show)
     logout conn
     disconnect conn

dropUntil cond (m@(ParseFailure _):msgs) dropped = dropUntil cond msgs (m:dropped)
dropUntil cond (m@(ParseSuccess msg):msgs) dropped = 
  if cond msg then (msg, msgs, reverse dropped) else dropUntil cond msgs (m:dropped)

playWithMmakowski = 
  do conn <- connect defaultFibsHost defaultFibsPort
     login conn "Habaź_v0.1.0" "habaztest_a" "habaztest"
     msgs <- readMessages conn
     let (ownInfo, msgs', dropped) = dropUntil isOwnInfo msgs []
     putStrLn (show ownInfo)
     putStrLn (show dropped)
     if ready ownInfo
       then putStrLn "ready already!"
       else do 
         sendCommand conn (Toggle Ready)
         let (system, msgs'', dropped') = dropUntil isSystem msgs' []
         putStrLn (show dropped')         
         putStrLn (show system)
     logout conn
     disconnect conn
  where
    isOwnInfo (OwnInfo _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
    isOwnInfo _ = False
    isSystem (System _) = True
    isSystem _ = False
    isFreeForm (FreeForm _) = True
    isFreeForm _ = False


-- tests

testAccount = "habaztest_a"
testPassword = "habaztest"

-- Note: this test will normally be disabled because there is no way to remove 
-- a test account from FIBS once it was created. If other tests fail because
-- the test account does not exist then enable this test to re-create the account
atest_createAccount = 
  do conn <- connect defaultFibsHost defaultFibsPort
     loginStatus <- login conn "habaz-test" testAccount testPassword
     result <- createTestAccount loginStatus conn
     disconnect conn
     return result
  where
    createTestAccount (LoginFailure "invalid login details") _ = assertBool "TODO" False
    createTestAccount (LoginFailure reason) _ = assertFailure reason
    createTestAccount LoginSuccess conn = 
      do
        logout conn
        assertFailure $ testAccount ++ " is already a valid FIBS account, can't test creation"
    
{-
test_accountCreation = 
  do conn <- connect defaultFibsHost defaultFibsPort
     --readUntil "login:" conn
     send conn "guest\n"
     str <- readUntil "!!!" conn
     -- putStr str
     send conn "bye\n"
     disconnect conn
     return $ assertBool "not logged in as guest" $ isInfixOf "You just logged in as guest" str

-}

-- helper test functions
toUTCTime str = (fromJust $ parseTime defaultTimeLocale "%s" str)



