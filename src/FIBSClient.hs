{-|
A high-level interface to FIBS. Usage:

* call 'connect' to establish a connection

* call 'login' to log in

* call 'readMessages' to obtain a list of parsed FIBS messages and a write-only version of connection. 
After this call the original read-write connection value should not be used.

* call 'logout' to log out; this is optional, since disconnecting will log you out anyway, and you should not
log in again using the same connection because FIBS behaves strangely if you do.

* call 'disconnect' to close the socket.
-}
module FIBSClient(
  -- * Constants
  clipVersion,
  defaultFIBSHost,
  defaultFIBSPort,
  -- * Connection Management
  Connection,
  ReadWriteConnection,
  WriteOnlyConnection,
  -- * Communication Data Types
  FIBSCommand (..),
  FIBSMessage (..),
  Flag (..),
  -- * Misc Data Types
  LoginStatus (..),
  ParseResult (..),
  -- * Functions
  connect,
  disconnect,
  login,
  logout,
  readMessages,
  sendCommand
) where
import FIBSClient.Commands
import FIBSClient.Messages
import Control.Applicative
import Control.Monad
import Data.List
import Network.Socket hiding (connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO
import Test.HUnit

-- * Constants

-- | the default FIBS host
defaultFIBSHost = "fibs.com"
-- | the default FIBS port
defaultFIBSPort = "4321"
-- | the version of FIBS CLIP supported by FIBSClient
clipVersion = "1008"


-- * Connection Management

class Connection a where
  handle :: a -> Handle

newtype ReadWriteConnection = RWConn Handle deriving (Eq, Show)
instance Connection ReadWriteConnection where handle (RWConn h) = h

newtype WriteOnlyConnection = WOConn Handle deriving (Eq, Show)
instance Connection WriteOnlyConnection where handle (WOConn h) = h

-- * Misc Data Types
                  
data LoginStatus = LoginSuccess
                 | LoginFailure String
                   

-- * Interface Functions

-- | Creates a connection to FIBS.
connect :: HostName               -- ^ The host to connect to
        -> String                 -- ^ Port number
        -> IO ReadWriteConnection -- ^ The opened connnection
connect hostname port =
  do addrinfos <- withSocketsDo $ getAddrInfo Nothing (Just hostname) (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     setSocketOption sock KeepAlive 1
     Network.Socket.connect sock (addrAddress serveraddr)
     h <- socketToHandle sock ReadWriteMode
     return $ RWConn h

-- | Disconnects from FIBS.
disconnect :: Connection c => c -> IO ()
disconnect conn = hClose $ handle conn

-- | Attempts to log in to FIBS.
login :: ReadWriteConnection     -- ^ a FIBS connection
      -> String                  -- ^ client name
      -> String                  -- ^ user name
      -> String                  -- ^ password
      -> IO LoginStatus          -- ^ the status of loggin attempt
login (RWConn h) clientname username password = 
  do readUntil ["login:"] h
     send h $ "login " ++ clientname ++ " " ++ clipVersion ++ " " ++ username ++ " " ++ password
     readUntil ["\n"] h
     line <- readUntil ["\n", "login:"] h
     return $ case fst (parseFIBSMessage line) of
       ParseFailure msg             -> LoginFailure $ "parse error: " ++ msg
       ParseSuccess LoginPrompt     -> LoginFailure "invalid login details"
       ParseSuccess (Welcome _ _ _) -> LoginSuccess
     
-- | Logs out from FIBS.
logout :: Connection c
       => c            -- ^ a FIBS connection
       -> IO ()
logout conn = send (handle conn) $ "bye"
    
-- | Yields a stream of parsed FIBS messages and makes the connection write-only.
readMessages :: ReadWriteConnection          -- ^ a FIBS connection
             -> IO ([ParseResult FIBSMessage], WriteOnlyConnection)
             -- ^ IO action wrapping a (lazy) list of parsed FIBS messages and the write-only
             -- oonnection that should be used from now on.
readMessages (RWConn h) = 
  do msgStr <- hGetContents h -- lazy!
     return $ (parseFIBSMessages msgStr, WOConn h)

-- | Sends a command to FIBS.
sendCommand :: WriteOnlyConnection   -- ^ a FIBS connection
            -> FIBSCommand           -- ^ the command to send
            -> IO ()
sendCommand (WOConn h) cmd = send h $ formatCommand cmd


-- communication primitives

-- | Reads from connection until one of supplied terminating strings is encountered.
readUntil :: [String]    -- ^ a list of terminators
          -> Handle      -- ^ handle to read from
          -> IO String   -- ^ the read string, including the terminator
readUntil termStrs h = liftM reverse $ loop []
  where
    rTermStrs = map reverse termStrs
    isTerminated rstr (rTermStr:rTermStrs) = isPrefixOf rTermStr rstr || isTerminated rstr rTermStrs
    isTerminated _ [] = False
    loop acc = do
      input <- hGetChar h
      --putStr $ if isTerminated (input:acc) rTermStrs then reverse (input:acc) else ""
      (if isTerminated (input:acc) rTermStrs then return else loop) (input:acc)
      
readLine ::Handle -> IO String
readLine = readUntil ["\n", "login:"] 

send :: Handle -> String -> IO ()
send h cmd =
  do hPutStrLn h cmd
     hFlush h

-- test helpers

-- experiments in progress:

test = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
     login conn "Habaź" "habaztest_a" "habaztest"
     (msgs, conn') <- readMessages conn
     take 30 <$> (return msgs) >>= mapM_ (putStrLn . show)
     logout conn'
     disconnect conn'

playWithMmakowski = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
     login conn "Habaź_v0.1.0" "habaztest_a" "habaztest"
     (msgs, conn') <- readMessages conn
     let (dropped, ownInfo, msgs') = msgs `splitByFirst` isOwnInfo
     putStrLn (show ownInfo)
     putStrLn (show dropped)
     if ready ownInfo
       then putStrLn "ready already!"
       else do 
         sendCommand conn' (Toggle Ready)
         let (dropped', system, msgs'') = msgs' `splitByFirst` isSystem
         putStrLn (show dropped')         
         putStrLn (show system)
     logout conn'
     disconnect conn'


-- tests

testAccount = "habaztest_a"
testPassword = "habaztest"

-- Note: this test will normally be disabled because there is no way to remove 
-- a test account from FIBS once it was created. If other tests fail because
-- the test account does not exist then enable this test to re-create the account
{-
atest_createAccount = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
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
-}    
{-
test_accountCreation = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
     --readUntil "login:" conn
     send conn "guest\n"
     str <- readUntil "!!!" conn
     -- putStr str
     send conn "bye\n"
     disconnect conn
     return $ assertBool "not logged in as guest" $ isInfixOf "You just logged in as guest" str

-}




