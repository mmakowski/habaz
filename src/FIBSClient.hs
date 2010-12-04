module FIBSClient where 
import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import System.IO

defaultFibsHost :: HostName
defaultFibsHost = "fibs.com"
defaultFibsPort :: String
defaultFibsPort = "4321"

type Command = String
type Connection = Handle

connect :: HostName             -- ^ The host to connect to
        -> String               -- ^ Port number
        -> IO Connection        -- ^ The opened connnection
connect hostname port =
  do addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Stream defaultProtocol
     setSocketOption sock KeepAlive 1
     Network.Socket.connect sock (addrAddress serveraddr)
     conn <- socketToHandle sock WriteMode
     return conn

-- login :: Handle

send :: Connection -> Command -> IO Connection
send conn cmd =
  do hPutStrLn conn cmd
     hFlush conn
     return conn

{--
syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogh fac pri msg =
    do hPutStrLn  sendmsg
       -- Make sure that we send data immediately
       hFlush (slHandle syslogh)
    where code = makeCode fac pri
          sendmsg = "<" ++ show code ++ ">" ++ (slProgram syslogh) ++
                    ": " ++ msg
--}

disconnect :: Connection -> IO ()
disconnect conn = hClose conn
