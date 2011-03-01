{-|
This module contains data types and functions modeling the state of FIBS session. There are the following
levels of state:

1. /Session/: the topmost level, represents the state of entire application. Contains data such as the 
list of erros, the connection (when connected), list of players (when connected) and match state (when playing).

2. /Match/: current match score, player ids, game state (if game is in progress)

3. /Game/: the board, who has the dice etc.

Levels from /Match/ down are pure, /Session/ level involves IO actions.
-}
module Session(
  -- * Types
  SessionState (..),
  -- * Constants
  initialSessionState,
  -- * Functions
  stateName,
  logError,
  withErrors
) where
import FIBSClient (Connection, dummyConnection)
import Test.HUnit hiding (errors)

class State a where
  stateName :: a -> String

data SessionState
     -- | Client is disconnected from the server
     = LoggedOut { errors :: [String] }
     -- | Client is connected and logged in but ready state has not been recognised yet  
     | LoggedIn { connection :: Connection
                , errors :: [String]
                }
     -- | The player is refusing games and can't invite
     | NotReady { connection :: Connection
                  -- TODO: players
                , errors :: [String]
                }
     -- | The player is ready to play -- can invite and receive invitations
     | Ready { connection :: Connection
               -- TODO: players
               -- TODO: invitations
             , errors :: [String]
             }
     -- TODO: other session states
     deriving (Eq, Show)

instance Session.State SessionState where
  stateName s = case s of
    LoggedOut _     -> "LoggedOut"
    LoggedIn _ _    -> "LoggedIn"
    NotReady _ _  -> "NotReady"
    Ready _ _     -> "Ready"
    
-- constants

-- | The session state at the start of the application
initialSessionState = LoggedOut []

-- state manipulation functions

logError :: String -> SessionState -> SessionState
logError e st = case st of
  (LoggedOut es)     -> LoggedOut (es ++ [e])
  (LoggedIn c es)    -> LoggedIn c (es ++ [e])  
  (NotReady c es)  -> NotReady c (es ++ [e])
  (Ready c es)     -> Ready c (es ++ [e])

test_logErrorAppendsAnError =
  do assertErrorAppended "LoggedOut" (LoggedOut e1)
     conn <- dummyConnection
     assertErrorAppended "LoggedIn" (LoggedIn conn e1)     
     assertErrorAppended "NotReady" (NotReady conn e1)
     assertErrorAppended "Ready" (Ready conn e1)
  where
    e1 = ["error 1"]
    assertErrorAppended stDesc st = assertEqual stDesc ["error 1", "error 2"] (errors (logError "error 2" st))


(LoggedOut _) `withErrors` es = LoggedOut es
(LoggedIn c _) `withErrors` es = LoggedIn c es
(NotReady c _) `withErrors` es = NotReady c es
(Ready c _) `withErrors` es = Ready c es

