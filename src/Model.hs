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
  -- ** Constants
  initialSessionState,
  -- * Direct manipulation
  withErrors,
  -- * Transitions
  SessionStateTransition,
  login, logout, disconnect, startProcessingMessages, recogniseNotReady, recogniseReady,
  logErrorIO
) where
import FIBSClient hiding (login, logout, disconnect, Flag (..))
import qualified FIBSClient (login, logout, disconnect, Flag (..))
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
                          , errors :: [String]
                          }
     -- | The player is refusing games and can't invite
     | NotReady { connection :: WriteOnlyConnection
                  -- TODO: players
                , errors :: [String]
                }
     -- | The player is ready to play -- can invite and receive invitations
     | Ready { connection :: WriteOnlyConnection
               -- TODO: players
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
    ProcessingMessages _ _ -> "ProcessingMessages"
    NotReady _ _  -> "NotReady"
    Ready _ _     -> "Ready"
    
-- ** Constants

-- | The session state at the start of the application
initialSessionState = Disconnected []

-- state manipulation functions

logError :: String -> SessionState -> SessionState
logError e st = case st of
  LoggedOut c es     -> LoggedOut c (es ++ [e])
  Disconnected es    -> Disconnected (es ++ [e])
  LoggedIn c m es    -> LoggedIn c m (es ++ [e])  
  ProcessingMessages c es -> ProcessingMessages c (es ++ [e])    
  NotReady c es  -> NotReady c (es ++ [e])
  Ready c es     -> Ready c (es ++ [e])

(LoggedOut c _) `withErrors` es = LoggedOut c es
(Disconnected _) `withErrors` es = Disconnected es
(LoggedIn c m _) `withErrors` es = LoggedIn c m es
(ProcessingMessages c _) `withErrors` es = ProcessingMessages c es
(NotReady c _) `withErrors` es = NotReady c es
(Ready c _) `withErrors` es = Ready c es


-- * Transitions

-- | Type alias to make it clear where a function returns a state transition. Session state transitions
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
login _ _ _ _ s = logErrorIO ("unable to login in " ++ (stateName s) ++ " state") s


-- | Logs out and disconnects from FIBS.
logout :: SessionStateTransition
logout s@(LoggedOut _ _) = logErrorIO "unable to logout in LoggedOut state" s
logout s@(Disconnected _) = logErrorIO "unable to logout in Disconnected state" s
logout s = do 
  let conn = connection s
  FIBSClient.logout conn
  return $ LoggedOut conn (errors s)


-- | Disconnects from FIBS.
disconnect :: SessionStateTransition
disconnect s@(Disconnected _) = logErrorIO "unable to disconnect in Disconnected state" s
disconnect s = do
  let conn = connection s
  FIBSClient.disconnect conn
  return $ Disconnected (errors s)    
  
-- | Indicates that messages are now being processed.
startProcessingMessages :: SessionStateTransition
startProcessingMessages (LoggedIn conn msgs e) = return $ ProcessingMessages conn e
startProcessingMessages s = logErrorIO ("unable to start processing messages in " ++ (stateName s) ++ " state") s

-- | Recognises that player is not ready
recogniseNotReady :: SessionStateTransition
recogniseNotReady (ProcessingMessages conn e) = return $ NotReady conn e
recogniseNotReady s = logErrorIO ("unable to recognise not ready state in " ++ (stateName s) ++ " state") s

-- | Recognises that player is ready
recogniseReady :: SessionStateTransition
recogniseReady (ProcessingMessages conn e) = return $ Ready conn e
recogniseReady s = logErrorIO ("unable to recognise ready state in " ++ (stateName s) ++ " state") s

-- | Disconnect

-- helper functions

logErrorIO :: String -> SessionStateTransition
logErrorIO e st = return $ logError e st
