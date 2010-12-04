module FIBSClient where 
import Char
import Control.Monad
import Data.Bits
import Data.List
import Network.Socket hiding (connect)
import qualified Network.Socket (connect)
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

test = 
  do conn <- connect defaultFibsHost defaultFibsPort
     readUntil '&' conn
     return $ disconnect conn

-- TODO: termStr should be a string; check if termStr is matched as the character are read
readUntil termStr conn = liftM reverse $ loop []
  where
    loop acc = do
      input <- hGetChar conn --fibsGetLine conn
      putStr $ [input] ++ ":" ++ (show $ Char.ord input) ++ " "
      (if input /= termStr then loop else return) (input:acc)
      