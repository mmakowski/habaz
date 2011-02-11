module FIBSClient where 
import Control.Monad
import Data.Bits
import Data.List
import Data.Time
import Network.Socket hiding (Connected, connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO


type Command = String

data Connection = Disconnected
                | Connected Handle
                  
data LoginStatus = Success
                 | Failure String
                   
data CLIPInputLine = FailedLogin
                   | Welcome String UTCTime String -- username, last login, last host

defaultFibsHost = "fibs.com"
defaultFibsPort = "4321"
clipVersion = "1008"

-- communication primitives

readUntil :: String -> Connection -> IO String
readUntil termStr (Connected conn) = liftM reverse $ loop []
  where
    rTermStr = reverse termStr
    loop acc = do
      input <- hGetChar conn
      putStr $ [input]
      (if isPrefixOf rTermStr (input:acc) then return else loop) (input:acc)
      
send :: Connection -> Command -> IO Connection
send conn@(Connected handle) cmd =
  do hPutStrLn handle cmd
     hFlush handle
     return conn


-- message parsing
     
parseLine :: String -> CLIPInputLine
parseLine = parseWords . words 
  where
    parseWords ("login:":_) = FailedLogin
    parseWords ["1", username, millisString, ip] = 
      Welcome username (UTCTime (ModifiedJulianDay 1) (secondsToDiffTime 1)) ip -- TODO: parse time
    -- TODO: other cases


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

login :: Connection     -- ^ An open conection
      -> String         -- ^ Client name
      -> String         -- ^ User name
      -> String         -- ^ Password
      -> IO LoginStatus -- ^ The status of loggin attempt
login conn@(Connected _) clientname username password = 
  do readUntil "login:" conn
     send conn $ "login " ++ clientname ++ " " ++ clipVersion ++ " " ++ username ++ " " ++ password
     readUntil"2 " conn
     return $ Failure "not implemented yet"
     
