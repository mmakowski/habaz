{-|
A high-level interface to FIBS. 
-}
module FIBSClient(
  FIBSCommand (..),
  Connection,
  FIBSMessage (..),
  Flag (..),
  ParseResult (..),
  clipVersion,
  connect,
  defaultFIBSHost,
  defaultFIBSPort,
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

type Connection = Handle
                  
data LoginStatus = LoginSuccess
                 | LoginFailure String
                   

-- constants

-- | the default FIBS host
defaultFIBSHost = "fibs.com"
-- | the default FIBS port
defaultFIBSPort = "4321"
-- | the version of FIBS CLIP supported by FIBSClient
clipVersion = "1008"

-- interface functions

-- | Creates a connection to FIBS.
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

-- | Disconnects from FIBS.
disconnect :: Connection -> IO ()
disconnect conn = hClose conn

-- | Attempts to log in to FIBS.
login :: Connection     -- ^ a FIBS connection
      -> String         -- ^ client name
      -> String         -- ^ user name
      -> String         -- ^ password
      -> IO LoginStatus -- ^ the status of loggin attempt
login conn clientname username password = 
  do readUntil ["login:"] conn
     send conn $ "login " ++ clientname ++ " " ++ clipVersion ++ " " ++ username ++ " " ++ password
     readUntil ["\n"] conn
     line <- readUntil ["\n", "login:"] conn
     return $ case fst (parseFIBSMessage line) of
       ParseFailure msg             -> LoginFailure $ "parse error: " ++ msg
       ParseSuccess FailedLogin     -> LoginFailure "invalid login details"
       ParseSuccess (Welcome _ _ _) -> LoginSuccess
     
-- | Logs out from FIBS.
logout :: Connection    -- ^ a FIBS connection
       -> IO ()
logout conn = send conn $ "bye"
    
-- | Yields a stream of parsed FIBS messages.
readMessages :: Connection                   -- ^ a FIBS connection
             -> IO [ParseResult FIBSMessage] -- ^ IO action wrapping a (lazy) list of parsed FIBS messages
readMessages conn = 
  do msgStr <- hGetContents conn -- lazy!
     return $ parseFIBSMessages msgStr

-- | Sends a command to FIBS.
sendCommand :: Connection   -- ^ a FIBS connection
            -> FIBSCommand  -- ^ the command to send
            -> IO ()
sendCommand conn cmd = 
  do send conn $ formatCommand cmd
     return ()

-- communication primitives

-- | Reads from connection until one of supplied terminating strings is encountered.
readUntil :: [String]    -- ^ a list of terminators
          -> Connection  -- ^ connection to read from
          -> IO String   -- ^ the read string, including the terminator
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


-- experiments in progress:

test = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
     login conn "Habaź" "habaztest_a" "habaztest"
     take 30 <$> readMessages conn >>= mapM_ (putStrLn . show)
     logout conn
     disconnect conn

playWithMmakowski = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
     login conn "Habaź_v0.1.0" "habaztest_a" "habaztest"
     msgs <- readMessages conn
     let (dropped, ownInfo, msgs') = msgs `splitByFirst` isOwnInfo
     putStrLn (show ownInfo)
     putStrLn (show dropped)
     if ready ownInfo
       then putStrLn "ready already!"
       else do 
         sendCommand conn (Toggle Ready)
         let (dropped', system, msgs'') = msgs' `splitByFirst` isSystem
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




