{-|
This module contains data types and functions modeling the state of FIBS session. There are the following
levels of state:

1. /Session/: the topmost level, represents the state of entire application. Contains data such as the 
list of erros, the connection (when connected), list of players (when connected) and match state (when playing).

2. /Match/: current match score, player ids, game state (if game is in progress)

3. /Game/: the board, who has the dice etc.

-}
module Model ( Session
             , (<|)
             , initialSession
             , disconnectedSession
             , loggedInSession
             , readySession
             ) 
where
import Events hiding (Disconnected)
import qualified Events as E (Event (Disconnected))

-- | State of a state machine; a is the type of state data and b is the type of event
-- that triggers state transition. A state is a pair of data and a transition function.
data State a b = State a (b -> State a b)
instance Show a => Show (State a b) where show (State d _) = show d
instance Eq a => Eq (State a b) where (State d1 _) == (State d2 _) = d1 == d2

-- TODO: how do we combine session state with other event consumers?

(<|) :: State a b -> b -> State a b
(State _ t) <| e = t e

-- | Session is a state of a state machine whose transitions are triggered by Events
type Session = State SessionData Event

-- | initial session state
initialSession :: Session
initialSession = disconnectedSession

-- | disconnected from server
disconnectedSession :: Session
disconnectedSession = State Disconnected disconnectedTransition

disconnectedTransition :: Event -> Session
disconnectedTransition e = case e of
  LoginSuccesful name -> loggedInSession name
  _                   -> disconnectedSession

-- | user is logged in but is not ready to play
loggedInSession :: String -> Session
loggedInSession name = State (LoggedIn name) (loggedInTransition name)

loggedInTransition :: String -> Event -> Session
loggedInTransition name e = disconnectionHandler e $ case e of
  ReadyOn        -> readySession name
  _              -> loggedInSession name

-- | user is ready to play
readySession :: String -> Session
readySession name = State (Ready name) (readyTransition name)

readyTransition :: String -> Event -> Session
readyTransition name e = disconnectionHandler e $ error "TODO: readyTransition"

disconnectionHandler :: Event -> Session -> Session
disconnectionHandler e s = case e of
  E.Disconnected -> disconnectedSession
  _              -> s


data SessionData
     = Disconnected
     | LoggedIn String
     | Ready String
     -- TODO: more states
  deriving (Eq, Show)

{-

data StateDataF
     -- | Client is disconnected from the server
     = Disconnected { errors :: [String] } 
     -- | Client is logged out but we still hold a connection which might need to be closed
     | LoggedOut { connection :: c
                 , errors :: [String] }
     -- | Client is connected and logged in but the messages from the server are not being processed yet
     | LoggedIn { connection :: c
                , messages :: [ParseResult FIBSMessage]
                , errors :: [String]
                }
     -- | The messages are being processed; ready state has not been recognised yet
     | ProcessingMessages { connection :: c
                          , players :: Players
                          , errors :: [String]
                          }
     -- | The player is refusing games and can't invite
     | NotReady { connection :: c
                , players :: Players
                , errors :: [String]
                }
     -- | The player is ready to play -- can invite and receive invitations
     | Ready { connection :: c
             , players :: Players
               -- TODO: invitations
             , errors :: [String]
             }
     -- TODO: other session states
     deriving (Eq, Show)

type StateData = StateDataF WriteOnlyConnection

instance State (SessionStateF c) where
  stateName s = case s of
    LoggedOut _ _   -> "LoggedOut"
    Disconnected _  -> "Disconnected"
    LoggedIn _ _ _    -> "LoggedIn"
    ProcessingMessages _ _ _ -> "ProcessingMessages"
    NotReady _ _ _ -> "NotReady"
    Ready _ _ _    -> "Ready"
    
type PlayerMap = Map PlayerName PlayerInfo

data Players = Players { playerMap :: PlayerMap
                       , playerDeltas :: [PlayerDelta] -- TODO: should be enough to have one -- each delta is handled immediately
                       }
               deriving (Eq, Show)

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
                   
-- | Player delta helps the view in figuring out what needs to be changed in the list of
-- players that it displays.
data PlayerDelta = Added PlayerName
                 | Removed PlayerName
                 | Updated PlayerName
                 deriving (Eq, Show)
 
pdstr :: PlayerDelta -> String
pdstr (Added pn) = pnstr pn
pdstr (Removed pn) = pnstr pn
pdstr (Updated pn) = pnstr pn
                         
instance Ord PlayerDelta where
  compare d1 d2 = compare (pdstr d1) (pdstr d2)
  
 

-- ** Constants

-- | The session state at the start of the application
initialSessionState = Disconnected []

-- state manipulation functions

logError :: String -> SessionStateF c -> SessionStateF c
logError e st = case st of
  LoggedOut c es     -> LoggedOut c (e:es)
  Disconnected es    -> Disconnected (e:es)
  LoggedIn c m es    -> LoggedIn c m (e:es)  
  ProcessingMessages c p es -> ProcessingMessages c p (e:es)    
  NotReady c p es  -> NotReady c p (e:es)
  Ready c p es     -> Ready c p (e:es)

popError :: SessionStateF c -> (Maybe String, SessionStateF c)
popError st = case st of
  LoggedOut c (e:es) -> (Just e, LoggedOut c es)
  LoggedOut _ []     -> (Nothing, st)
  Disconnected (e:es) -> (Just e, Disconnected es)
  Disconnected [] -> (Nothing, st)
  LoggedIn c m (e:es) -> (Just e, LoggedIn c m es)
  LoggedIn _ _ [] -> (Nothing, st)
  ProcessingMessages c p (e:es) -> (Just e, ProcessingMessages c p es)
  ProcessingMessages _ _ [] -> (Nothing, st)
  NotReady c p (e:es) -> (Just e, NotReady c p es)
  NotReady _ _ [] -> (Nothing, st)
  Ready c p (e:es) -> (Just e, Ready c p es)
  Ready _ _ [] -> (Nothing, st)

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

-- | Tries to create an account. Has to be executed in a Disconnected state
-- TODO: provide a convenient way to deremine if the creation was succesful.
createAccount :: String                 -- ^ host
              -> String                 -- ^ port
              -> String                 -- ^ user name
              -> String                 -- ^ password
              -> SessionStateTransition -- ^ the state transition
createAccount host port userName password s@(Disconnected _) = do
  status <- FIBSClient.createAccount defaultFIBSHost defaultFIBSPort userName password
  case status of
    AccountCreationSuccess -> return s
    AccountCreationFailure msg -> return $ logError msg s
  
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
updatePlayer p = do
  let pname = name p
      mapOp = Map.insert pname p 
      delta = Updated pname
  playerChange mapOp delta "update player info"

-- | Removes player
removePlayer :: PlayerName -> SessionStateTransition
removePlayer pname = do
  let mapOp = Map.delete pname
      delta = Removed pname
  playerChange mapOp delta "remove player"
  
-- helper functions

playerChange :: (PlayerMap -> PlayerMap) -> PlayerDelta -> String -> SessionStateTransition
playerChange mapOp delta _ s@(ProcessingMessages _ _ _) = playerChange' mapOp delta s 
playerChange mapOp delta _ s@(Ready _ _ _) = playerChange' mapOp delta s
playerChange mapOp delta _ s@(NotReady _ _ _) = playerChange' mapOp delta s
playerChange _ _ desc s = logUnableToErrorIO desc s
playerChange' mapOp delta s = do
  let ps = players s
      pm = mapOp $ playerMap ps
      pd = [delta]
      ps' = Players pm pd
  return $ s `withPlayers` ps'

logErrorIO :: String -> SessionStateTransition
logErrorIO e st = return $ logError e st

logUnableToErrorIO :: String -> SessionStateTransition
logUnableToErrorIO act s = logErrorIO ("unable to " ++ act ++ " in " ++ stateName s ++ " state") s
-}