module FIBSClient where 
import Char
import Control.Monad
import Data.Bits
import Data.List
import Network.Socket hiding (connect, send)
import qualified Network.Socket (connect, send)
import Network.BSD
import System.IO


type Command = String
type Connection = Handle

defaultFibsHost = "fibs.com"
defaultFibsPort = "4321"

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
     return conn

-- login :: Handle

send :: Connection -> Command -> IO Connection
send conn cmd =
  do hPutStrLn conn cmd
     hFlush conn
     return conn

disconnect :: Connection -> IO ()
disconnect conn = hClose conn

readUntil termStr conn = liftM reverse $ loop []
  where
    rTermStr = reverse termStr
    loop acc = do
      input <- hGetChar conn
      putStr $ [input]
      (if isPrefixOf rTermStr (input:acc) then return else loop) (input:acc)
      