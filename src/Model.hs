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
  SessionState (..),
  Players (..),
  PlayerName (PlayerName), pnstr,
  PlayerInfo (..),
  PlayerGameState (..),
  PlayerDelta (..),
  -- ** Constants
  initialSessionState,
  -- * Direct manipulation
  withErrors,
  -- * Transitions
  SessionStateTransition,
  login, logout, disconnect, startProcessingMessages, recogniseNotReady, recogniseReady, toggleReady,
  updatePlayer,
  logErrorIO
) where
import FIBSClient hiding (login, logout, disconnect, Flag (..), name)
import qualified FIBSClient (login, logout, disconnect, Flag (..), name)
-- player map
import Data.Map (Map)
import qualified Data.Map as Map
-- exception handling
import System.IO
import System.IO.Error

-- * States

class State a where
  stateName :: a -> String

data SessionState
     -- | Client is disconnected from the server
     = Disconnected { errors :: [String] } 
     -- | Client is logged out but we still hold a connection which might need to be closed
     | LoggedOut { connection :: WriteOnlyConnection
                 , errors :: [String] }
     -- | Client is connected and logged in but the messages from the server are not being processed yet
     | LoggedIn { connection :: WriteOnlyConnection
                , messages :: [ParseResult FIBSMessage]
                , errors :: [String]
                }
     -- | The messages are being processed; ready state has not been recognised yet
     | ProcessingMessages { connection :: WriteOnlyConnection
                          , players :: Players
                          , errors :: [String]
                          }
     -- | The player is refusing games and can't invite
     | NotReady { connection :: WriteOnlyConnection
                , players :: Players
                , errors :: [String]
                }
     -- | The player is ready to play -- can invite and receive invitations
     | Ready { connection :: WriteOnlyConnection
             , players :: Players
               -- TODO: invitations
             , errors :: [String]
             }
     -- TODO: other session states
     deriving (Eq, Show)

instance State SessionState where
  stateName s = case s of
    LoggedOut _ _   -> "LoggedOut"
    Disconnected _  -> "Disconnected"
    LoggedIn _ _ _    -> "LoggedIn"
    ProcessingMessages _ _ _ -> "ProcessingMessages"
    NotReady _ _ _ -> "NotReady"
    Ready _ _ _    -> "Ready"
    
data Players = Players { playerMap :: Map PlayerName PlayerInfo
                       , playerDeltas :: [PlayerDelta]
                       }
               deriving (Eq, Show)

newtype PlayerName = PlayerName String
                   deriving (Eq, Ord, Show)

pnstr :: PlayerName -> String
pnstr (PlayerName s) = s

-- | Player delta helps the view in figuring out what needs to be changed in the list of
-- players that it displays.
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
                   
data PlayerDelta = Added PlayerName
                 | Removed PlayerName
                 | Updated PlayerName
                 deriving (Eq, Show)
                          

-- ** Constants

-- | The session state at the start of the application
initialSessionState = Disconnected []

-- state manipulation functions

logError :: String -> SessionState -> SessionState
logError e st = case st of
  LoggedOut c es     -> LoggedOut c (es ++ [e])
  Disconnected es    -> Disconnected (es ++ [e])
  LoggedIn c m es    -> LoggedIn c m (es ++ [e])  
  ProcessingMessages c p es -> ProcessingMessages c p (es ++ [e])    
  NotReady c p es  -> NotReady c p (es ++ [e])
  Ready c p es     -> Ready c p (es ++ [e])

withErrors :: SessionState -> [String] -> SessionState
(LoggedOut c _) `withErrors` es = LoggedOut c es
(Disconnected _) `withErrors` es = Disconnected es
(LoggedIn c m _) `withErrors` es = LoggedIn c m es
(ProcessingMessages c ps _) `withErrors` es = ProcessingMessages c ps es
(NotReady c ps _) `withErrors` es = NotReady c ps es
(Ready c ps _) `withErrors` es = Ready c ps es

withPlayers :: SessionState -> Players -> SessionState
(ProcessingMessages c _ es) `withPlayers` ps = ProcessingMessages c ps es
(NotReady c _ es) `withPlayers` ps = NotReady c ps es
(Ready c _ es) `withPlayers` ps = Ready c ps es


-- * Transitions

-- | Type alias to make it clear where a function returns a state transition. Session state transitionsx
-- might involve IO actions hence the result type is tainted with IO.
type SessionStateTransition = SessionState -> IO SessionState

-- | Tries to connect and log in to FIBS.
login :: String                  -- ^ host
      -> String                  -- ^ port
      -> String                  -- ^ user name
      -> String                  -- ^ password
      -> SessionStateTransition  -- ^ the state transition
login host port userName password s@(Disconnected es) = connectAndLogin `catch` errorHandler
  where
    connectAndLogin = 
      do conn <- connect host port
         loginStatus <- FIBSClient.login conn "Habaź_v0.1.0" userName password
         case loginStatus of
           LoginFailure e -> do FIBSClient.disconnect conn
                                logErrorIO e s
           LoginSuccess   -> do (msgs, conn') <- readMessages conn
                                return $ LoggedIn conn' msgs es
    errorHandler e = logErrorIO ("error connecting to " ++ host ++ ":" ++ port) s
login _ _ _ _ s = logUnableToErrorIO "login" s

-- | Logs out from FIBS.
logout :: SessionStateTransition
logout s@(LoggedOut _ _) = logUnableToErrorIO "logout" s
logout s@(Disconnected _) = logUnableToErrorIO "logout" s
logout s = do 
  let conn = connection s
  FIBSClient.logout conn
  return $ LoggedOut conn (errors s)

-- | Disconnects from FIBS.
disconnect :: SessionStateTransition
disconnect s@(Disconnected _) = logUnableToErrorIO "disconnect" s
disconnect s = do
  let conn = connection s
  FIBSClient.disconnect conn
  return $ Disconnected (errors s)    
  
-- | Indicates that messages are now being processed.
startProcessingMessages :: SessionStateTransition
startProcessingMessages (LoggedIn conn msgs e) = 
  return $ ProcessingMessages conn (Players Map.empty []) e
startProcessingMessages s = logUnableToErrorIO "start processing messages" s

-- | Recognises that player is not ready
recogniseNotReady :: SessionStateTransition
recogniseNotReady (ProcessingMessages conn p e) = return $ NotReady conn p e
recogniseNotReady s = logUnableToErrorIO "recognise not ready state" s

-- | Recognises that player is ready
recogniseReady :: SessionStateTransition
recogniseReady (ProcessingMessages conn p e) = return $ Ready conn p e
recogniseReady s = logUnableToErrorIO "recognise ready state" s

-- | Toggles ready state
toggleReady :: SessionStateTransition
toggleReady s@(Ready _ _ _) = toggleReady' s
toggleReady s@(NotReady _ _ _) = toggleReady' s
toggleReady s = logUnableToErrorIO "toggle ready state" s
toggleReady' s = do 
  let conn = connection s
  sendCommand conn (Toggle FIBSClient.Ready)
  return $ ProcessingMessages conn (players s) (errors s)

-- | Updates player info
updatePlayer :: PlayerInfo -> SessionStateTransition
updatePlayer p s@(ProcessingMessages _ _ _) = updatePlayer' p s 
updatePlayer p s@(Ready _ _ _) = updatePlayer' p s
updatePlayer p s@(NotReady _ _ _) = updatePlayer' p s
updatePlayer _ s = logUnableToErrorIO "update player info" s
updatePlayer' p s = do
  let pname = name p
      ps = players s
      pm = Map.insert pname p (playerMap ps) 
      pd = (Updated pname):(playerDeltas ps)
      ps' = Players pm pd
  return $ s `withPlayers` ps'

-- helper functions

logErrorIO :: String -> SessionStateTransition
logErrorIO e st = return $ logError e st

logUnableToErrorIO :: String -> SessionStateTransition
logUnableToErrorIO act s = logErrorIO ("unable to " ++ act ++ " in " ++ (stateName s) ++ " state") s
