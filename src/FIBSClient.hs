module FIBSClient where 
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import Data.Time
import Network.Socket hiding (Connected, connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO
import System.Locale


type Command = String

-- TODO: manage connection state more thoroughly
data Connection = Disconnected
                | Connected Handle
                  
data LoginStatus = Success
                 | Failure String
                   
data CLIPMessage = ParseFailure String
                   | FailedLogin
                   | Welcome String UTCTime String -- username, last login, last host
                   deriving (Eq, Show)


defaultFibsHost = "fibs.com"
defaultFibsPort = "4321"
clipVersion = "1008"

-- communication primitives

readUntil :: [String] -> Connection -> IO String
readUntil termStrs (Connected conn) = liftM reverse $ loop []
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

send :: Connection -> Command -> IO Connection
send conn@(Connected handle) cmd =
  do hPutStrLn handle cmd
     hFlush handle
     return conn


-- message parsing
     
parseLine :: String -> CLIPMessage
parseLine = parseWords . words 
  where
    parseUTCTime = parseTime defaultTimeLocale "%s"
    parseWords ("login:":_) = FailedLogin
    parseWords ["1", username, millisString, ip] = case parseUTCTime millisString of
      Just(time) -> Welcome username time ip
      Nothing -> ParseFailure $ "unable to parse " ++ millisString ++ " as time"
    -- TODO: other cases
    parseWords words = ParseFailure $ "unable to parse: " ++ (unwords words)

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
     return $ Connected conn

disconnect :: Connection -> IO ()
disconnect (Connected conn) = hClose conn

login :: Connection     -- ^ An open connection
      -> String         -- ^ Client name
      -> String         -- ^ User name
      -> String         -- ^ Password
      -> IO LoginStatus -- ^ The status of loggin attempt
login conn@(Connected _) clientname username password = 
  do readUntil ["login:"] conn
     send conn $ "login " ++ clientname ++ " " ++ clipVersion ++ " " ++ username ++ " " ++ password
     readUntil ["\n"] conn
     line <- readUntil ["\n", "login:"] conn
     return $ case parseLine line of
       FailedLogin         -> Failure "invalid login details"
       ParseFailure reason -> Failure reason
       Welcome _ _ _       -> Success
     
logout :: Connection    -- ^ An open connection
       -> IO ()
logout conn@(Connected _) = 
  do send conn $ "bye"
     return ()
    
     
-- play in progress:

readMessages :: Connection -> IO [CLIPMessage]
--readMessages conn@(Connected _) = (:) <$> parseLine <$> readLine conn <*> readMessages conn
{-
readMessages conn@(Connected _) = 
  do
    lineStr <- readLine conn
    rest <- readMessages conn
    return (parseLine lineStr : rest)
-}

readMessages conn@(Connected h) = 
  do
    msgStr <- hGetContents h
    return $ parseLines msgStr
  where
    parseLines str = 
      let (first, rest) = getFirstLineStr str in (parseLine first : parseLines rest)
    getFirstLineStr str = 
      let (revFirst, rest) = loop [] str in (reverse revFirst, rest)
    rTermStrs = map reverse ["\n", "login:"]
    isTerminated rstr (rTermStr:rTermStrs) = isPrefixOf rTermStr rstr || isTerminated rstr rTermStrs
    isTerminated _ [] = False
    loop acc [] = (acc, [])
    loop acc (h:t) = 
      if isTerminated (h:acc) rTermStrs then ((h:acc), t) else loop (h:acc) t
    
    
test = 
  do conn <- connect defaultFibsHost defaultFibsPort
     login conn "Habaź" "habaztest_a" "habaztest"
     take 10 <$> readMessages conn >>= putStrLn . show
     logout conn
     disconnect conn
