module FIBSClient where -- TODO: limit exports
import FIBSClient.Messages
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Time
import Network.Socket hiding (connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO
import Test.HUnit
import Test.QuickCheck


type Connection = Handle
                  
data LoginStatus = LoginSuccess
                 | LoginFailure String
                   
data Flag
     = Ready
     deriving (Eq, Show)

data Command
     = Toggle Flag
     deriving (Eq, Show)

instance Test.QuickCheck.Arbitrary Command where
  arbitrary = elements [Toggle Ready] -- TODO: more examples
  coarbitrary _ = id -- TODO


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




