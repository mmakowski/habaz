module FIBSClient where 
import Control.Applicative
import Control.Monad
import CLIP hiding (ParseResult (..), login)
import qualified CLIP (ParseResult (..))
import Data.Bits
import Data.List
import Network.Socket hiding (Connected, connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO


-- TODO: manage connection state more thoroughly
data Connection = Disconnected
                | Connected Handle
                  
data LoginStatus = Success
                 | Failure String
                   


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

send :: Connection -> String -> IO Connection
send conn@(Connected handle) cmd =
  do hPutStrLn handle cmd
     hFlush handle
     return conn


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
     return $ case fst (parseCLIPMessage line) of
       CLIP.Failure msg             -> Failure $ "parse error: " ++ msg
       CLIP.Success FailedLogin     -> Failure "invalid login details"
       CLIP.Success (Welcome _ _ _) -> Success
     
logout :: Connection    -- ^ An open connection
       -> IO ()
logout conn@(Connected _) = 
  do send conn $ "bye"
     return ()
    

readMessages :: Connection -> IO [CLIP.ParseResult CLIPMessage]
readMessages (Connected h) = 
  do
    msgStr <- hGetContents h -- lazy!
    return $ parseCLIPMessages msgStr

sendCommand :: Connection -> Command -> IO ()
sendCommand conn@(Connected _) cmd = 
  do send conn $ formatCommand cmd
     return ()

-- play in progress:


test = 
  do conn <- connect defaultFibsHost defaultFibsPort
     login conn "Habaź" "habaztest_a" "habaztest"
     take 30 <$> readMessages conn >>= putStrLn . show
     logout conn
     disconnect conn

dropUntil cond (m@(CLIP.Failure _):msgs) dropped = dropUntil cond msgs (m:dropped)
dropUntil cond (m@(CLIP.Success msg):msgs) dropped = 
  if cond msg then (msg, msgs, reverse dropped) else dropUntil cond msgs (m:dropped)

playWithMmakowski = 
  do conn <- connect defaultFibsHost defaultFibsPort
     login conn "Habaź_v0.1.0" "habaztest_a" "habaztest"
     msgs <- readMessages conn
     let (ownInfo, msgs', dropped) = dropUntil isOwnInfo msgs []
     putStrLn (show ownInfo)
     putStrLn (show dropped)
     if not (ready ownInfo)
       then putStrLn "ready already!"
       else do 
         sendCommand conn (Toggle Ready)
         let (system, msgs'', dropped') = dropUntil isSystem msgs' []
         putStrLn (show system)
         putStrLn (show dropped')         
     logout conn
     disconnect conn
  where
    isOwnInfo (OwnInfo _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
    isOwnInfo _ = False
    isSystem (System _) = True
    isSystem _ = False
    isFreeForm (FreeForm _) = True
    isFreeForm _ = False
