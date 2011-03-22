{-|
This module contains data types and functions modeling the state of FIBS session. There are the following
levels of state:

1. /Session/: the topmost level, represents the state of entire application. Contains data such as the 
list of erros, the connection (when connected), list of players (when connected) and match state (when playing).

2. /Match/: current match score, player ids, game state (if game is in progress)

3. /Game/: the board, who has the dice etc.

Levels from /Match/ down are pure, /Session/ level involves IO actions.
-}
module Model(
  -- * States
  Disconnected, LoggedOut, NotProcessingMessages, ProcessingMessages, NotReady, NotPlaying,
  -- * Errors
  TransitionError (..),
  -- * Transitions
  login, logout, disconnect, startProcessingMessages, recogniseNotReady, recogniseReady, setNotReady,
  setReady, updateOrAddPlayer
) where
import FIBSClient hiding (login, logout, disconnect, Flag (..), LoginStatus (..), name)
import qualified FIBSClient (login, logout, disconnect, Flag (..), LoginStatus (..), name)
-- player map
import Data.Map (Map)
import qualified Data.Map as Map


-- * States

-- ** Classes

-- | All states where the session is connected.
class Connected s where
  connection :: s -> WriteOnlyConnection

-- | All states where the session is logged in.
class Connected s => LoggedIn s

-- | All states where there is information about players.
class WithPlayers s where
  players :: s -> Players
  withPlayers :: s -> Players -> s
  
-- | All states where the player is ready to play.
class (Connected s, WithPlayers s) => Ready s


-- ** Types

-- | Client is disconnected.
data Disconnected = Disconnected
                  deriving (Eq, Show)

-- | Client is logged out but we still hold a connection which might need to be closed.
data LoggedOut = LoggedOut { loConn :: WriteOnlyConnection }
                 deriving (Eq, Show)
instance Connected LoggedOut where connection = loConn
  
-- | Client is connected and logged in but the messages from the server are not being processed yet.
data NotProcessingMessages = NotProcessingMessages { npmConn :: WriteOnlyConnection 
                                                   , messages :: [ParseResult FIBSMessage]
                                                   }
                           deriving (Eq, Show)
instance Connected NotProcessingMessages where connection = npmConn
instance LoggedIn NotProcessingMessages

-- | The messages are being processed; ready state has not been recognised yet.
data ProcessingMessages = ProcessingMessages { pmConn :: WriteOnlyConnection 
                                             , pmPlayers :: Players
                                             }
                        deriving (Eq, Show)
instance Connected ProcessingMessages where connection = pmConn
instance LoggedIn ProcessingMessages                                            
instance WithPlayers ProcessingMessages where 
  players = pmPlayers
  withPlayers s ps = s { pmPlayers = ps }
                                              
-- | The player is refusing games and can't invite.
data NotReady = NotReady { nrConn :: WriteOnlyConnection
                         , nrPlayers :: Players
                         }
                deriving (Eq, Show)
instance Connected NotReady where connection = nrConn
instance LoggedIn NotReady
instance WithPlayers NotReady where 
  players = nrPlayers
  withPlayers s ps = s { nrPlayers = ps }

-- | The player is ready to play but not playing.
data NotPlaying = NotPlaying { npConn :: WriteOnlyConnection
                             , npPlayers :: Players
                             }
                deriving (Eq, Show)
instance Connected NotPlaying where connection = npConn
instance LoggedIn NotPlaying
instance WithPlayers NotPlaying where 
  players = npPlayers
  withPlayers s ps = s { npPlayers = ps }
instance Ready NotPlaying


-- * Data stored in states

type Players = Map PlayerName PlayerInfo

newtype PlayerName = PlayerName String
                   deriving (Eq, Ord, Show)

pnstr :: PlayerName -> String
pnstr (PlayerName s) = s

data PlayerInfo = PlayerInfo { name :: PlayerName
                             , ready :: Bool
                             , playerGameState :: PlayerGameState
                             , rating :: Float
                             , experience :: Int
                             } 
                deriving (Eq, Show)

data PlayerGameState = None
                     | Playing String 
                     | Watching String
                     deriving (Eq, Show)
                   

-- * Errors

data TransitionError = LoginFailure { errMsg :: String }
                     | ConnectionFailure { errHost :: String, errPort :: String }
                     deriving (Eq, Show)


-- * Transitions

-- | Tries to connect and log in to FIBS.
login :: String                  -- ^ host
      -> String                  -- ^ port
      -> String                  -- ^ user name
      -> String                  -- ^ password
      -> Disconnected            -- ^ current state
      -> IO (Either TransitionError NotProcessingMessages)
login host port userName password Disconnected = connectAndLogin `catch` errorHandler
  where
    connectAndLogin = do 
      conn <- connect host port
      loginStatus <- FIBSClient.login conn "Habaź_v0.1.0" userName password
      case loginStatus of
        FIBSClient.LoginFailure e -> do FIBSClient.disconnect conn
                                        return $ Left $ LoginFailure e
        FIBSClient.LoginSuccess   -> do (msgs, conn') <- readMessages conn
                                        return $ Right $ NotProcessingMessages conn' msgs
    errorHandler _ = return $ Left $ ConnectionFailure host port

-- | Logs out from FIBS.
logout :: LoggedIn s => s -> IO LoggedOut 
logout s = do 
  let conn = connection s
  FIBSClient.logout conn
  return $ LoggedOut conn

-- | Disconnects from FIBS.
disconnect :: Connected s => s -> IO Disconnected
disconnect s = do
  FIBSClient.disconnect $ connection s
  return $ Disconnected

-- | Indicates that messages are now being processed.
startProcessingMessages :: NotProcessingMessages -> ProcessingMessages
startProcessingMessages (NotProcessingMessages conn msgs) = 
  ProcessingMessages conn Map.empty

-- | Recognises that player is not ready.
recogniseNotReady :: ProcessingMessages -> NotReady
recogniseNotReady (ProcessingMessages conn ps) = NotReady conn ps

-- | Recognises that player is ready.
recogniseReady :: ProcessingMessages -> NotPlaying
recogniseReady (ProcessingMessages conn ps) = NotPlaying conn ps

-- | Attempts to set player's ready status to False. 
setNotReady :: Ready s => s -> IO ProcessingMessages
setNotReady s = toggleReady (connection s) (players s)

-- | Attempts to set player's ready status to True. 
setReady :: NotReady -> IO ProcessingMessages
setReady (NotReady conn ps) = toggleReady conn ps

-- | Updates or adds player info.
updateOrAddPlayer :: WithPlayers s => PlayerInfo -> s -> s
updateOrAddPlayer p s = s `withPlayers` Map.insert (name p) p (players s)


-- ** Helper functions used in transitions

toggleReady :: WriteOnlyConnection -> Players -> IO ProcessingMessages
toggleReady conn ps = do
  sendCommand conn (Toggle FIBSClient.Ready)
  return $ ProcessingMessages conn ps

