module FIBSClient(
  LoginStatus(Success, Failure),
  connect,
  defaultFibsHost,
  defaultFibsPort,
  disconnect,
  login) where 
import Control.Monad
import Data.Bits
import Data.List
import Network.Socket hiding (Connected, connect, send)
import qualified Network.Socket (connect)
import Network.BSD
import System.IO


type Command = String
data Connection = Disconnected Handle
                | Connected Handle
data LoginStatus = Success
                 | Failure String

defaultFibsHost = "fibs.com"
defaultFibsPort = "4321"

-- communication primitives

readUntil :: String -> Connection -> IO String
readUntil termStr (Connected conn) = liftM reverse $ loop []
  where
    rTermStr = reverse termStr
    loop acc = do
      input <- hGetChar conn
      -- putStr $ [input]
      (if isPrefixOf rTermStr (input:acc) then return else loop) (input:acc)
      
send :: Connection -> Command -> IO Connection
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

login :: Connection     -- ^ An open conection
      -> String         -- ^ User name
      -> String         -- ^ Password
      -> IO LoginStatus -- ^ The status of loggin attempt
login (Connected conn) username password = 
  do return $ Failure "not implemented yet"

