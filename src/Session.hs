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
import FIBSClient (Connection, ParseResult, FIBSMessage, dummyConnection)
import Test.HUnit hiding (errors)

class State a where
  stateName :: a -> String

data SessionState
     = LoggedOut { errors :: [String] }
     | NotReady { connection :: Connection
                , messages :: [ParseResult FIBSMessage]
                  -- TODO: players
                , errors :: [String]
                }
     | Ready { connection :: Connection
             , messages :: [ParseResult FIBSMessage]               
               -- TODO: players
               -- TODO: invitations
             , errors :: [String]
             }
     -- TODO: other session states
     deriving (Eq, Show)

instance Session.State SessionState where
  stateName s = case s of
    LoggedOut _     -> "LoggedOut"
    NotReady _ _ _  -> "NotReady"
    Ready _ _ _     -> "Ready"
    
-- constants

-- | The session state at the start of the application
initialSessionState = LoggedOut []

-- state manipulation functions

logError :: String -> SessionState -> SessionState
logError e st = case st of
  (LoggedOut es)     -> LoggedOut (es ++ [e])
  (NotReady c m es)  -> NotReady c m (es ++ [e])
  (Ready c m es)     -> Ready c m (es ++ [e])

test_logErrorAppendsAnError =
  do assertErrorAppended "LoggedOut" (LoggedOut e1)
     conn <- dummyConnection
     assertErrorAppended "NotReady" (NotReady conn [] e1)
     assertErrorAppended "Ready" (Ready conn [] e1)
  where
    e1 = ["error 1"]
    assertErrorAppended stDesc st = assertEqual stDesc ["error 1", "error 2"] (errors (logError "error 2" st))


(LoggedOut _) `withErrors` es = LoggedOut es
(NotReady c m _) `withErrors` es = NotReady c m es
(Ready c m _) `withErrors` es = Ready c m es

