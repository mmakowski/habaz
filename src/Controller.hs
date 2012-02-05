{-
-}
module Controller(
  controller
) where
import Model
import View
import FIBSClient (defaultFIBSHost, defaultFIBSPort)
-- for mapping messages to updates
import FIBSClient.Messages hiding (name, opponent, watching, ready, rating, experience)
import qualified FIBSClient.Messages as Msg (name, opponent, watching, ready, rating, experience)
-- STM for session state:
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM (atomically)
-- async processing: 
import Control.Concurrent (forkIO)
-- message processing thread:
import Control.Monad (forever)
import Data.IORef

-- * Composing Model transitions and View updates

type ModelAndViewUpdate = TMVar SessionState -> View -> IO ()

type StateDependentViewUpdate = SessionState -> ViewUpdate ()

-- | A StateDependentViewUpdate that doesn't do anything
noOpSDVU :: StateDependentViewUpdate
noOpSDVU _ _ = return ()

-- | Combines a session state transition with a view update
(<>) :: SessionStateTransition -> StateDependentViewUpdate -> ModelAndViewUpdate
(<>) sessTrans viewUpd sessTV view = do
  sess' <- executeTransition sessTrans sessTV
  viewUpd sess' view

-- | Executes a transition by applying it to the state held in TMVar and updating the
-- contents of this TMVar.
executeTransition :: SessionStateTransition  -- ^ transition to execute
                  -> TMVar SessionState      -- ^ state reference
                  -> IO SessionState         -- ^ updated state in IO context
executeTransition sessTrans sessTV = do
  sess <- atomically $ takeTMVar sessTV
  sess' <- sessTrans sess
  atomically $ putTMVar sessTV sess'
  return sess'

-- ** Simple updates

logoutU :: ModelAndViewUpdate
logoutU = logout <> (\s -> disableLogOut |> disableReady |> showInfoMessage (show s))

exitU :: ModelAndViewUpdate
exitU _ = closeMainWindow

disconnectU :: ModelAndViewUpdate
disconnectU = disconnect <> (\s -> enableLogIn |> disableLogOut |> disableReady)

toggleReadyU :: ModelAndViewUpdate
toggleReadyU = toggleReady <> noOpSDVU

noOpU :: ModelAndViewUpdate
noOpU _ _ = return ()

-- ** Complex commands

loginU :: ModelAndViewUpdate
loginU sessTV view = do
  maybeUp <- promptForUsernameAndPassword view
  case maybeUp of
    Just (username, password) -> doLogin username password -- TODO: validate that they are not empty
    Nothing -> return ()
  where
    doLogin username password = do
      disableLogIn view
      sess' <- executeTransition (login defaultFIBSHost defaultFIBSPort username password) sessTV
      case sess' of
        (Disconnected _) -> promptToCreateAccount username password
        (LoggedIn _ m _) -> do startMessageProcessingThread m
                               (disableLogIn |> enableLogOut) view
    promptToCreateAccount username password = do 
      errorMsg <- popErrorTV sessTV
      let prefix = case errorMsg of
                     Just msg -> "Login failed: " ++ msg ++ ". "
                     Nothing -> "Login failed for unknown reason. "
      create <- promptYesNo (prefix ++ " Would you like to create an account with supplied credentials?") view
      if create 
        then do
          executeTransition (createAccount defaultFIBSHost defaultFIBSPort username password) sessTV
          -- TODO: display error if account creation failed
          doLogin username password
        else enableLogIn view
    startMessageProcessingThread msgs = do 
      executeTransition startProcessingMessages sessTV
      forkIO $ processMessage msgs `catch` errorHandler
    processMessage (msg:msgs) = do
      print msg -- debugging only
      updateForMessage msg sessTV view
      if isTerminating msg then disconnectU sessTV view
        else processMessage msgs
    errorHandler _ = do
      -- this is typically executed when a session is logged out when the server disconnects after a log
      -- out but there is a backlog of messages our end and the message processing thread has not received
      -- a terminating message. So far I only noticed it happening when a Log Out command is issued shortly
      -- after a login. In this scenario we want to disconnect anyway so let's disconnect without reporting 
      -- any errors to the user.
      putStrLn "disconnecting in error handler"
      disconnectU sessTV view 
    
-- ** Model and View Updates for FIBS messages

-- | Yields ModelAndViewUpdate corresponding to given FIBSMessage
updateForMessage :: ParseResult FIBSMessage -> ModelAndViewUpdate
updateForMessage (ParseFailure err) = 
  \sessTV view -> do executeTransition (logErrorIO err) sessTV
                     reportErrors sessTV view
updateForMessage (ParseSuccess msg) = case msg of
  Logout _ _ -> removePlayerU msg
  OwnInfo _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> recogniseReadyU $ Msg.ready msg
  ReadyOn -> recogniseReadyU True
  ReadyOff -> recogniseReadyU False
  WhoInfo _ _ _ _ _ _ _ _ _ _ _ _ -> updatePlayerU msg
  _ -> noOpU
  
recogniseReadyU :: Bool -> ModelAndViewUpdate
recogniseReadyU ready sessTV view = do
  executeTransition (if ready then recogniseReady else recogniseNotReady) sessTV
  (enableReady |> setCheckedReady ready) view

removePlayerU :: FIBSMessage -> ModelAndViewUpdate
removePlayerU (Logout pName msg) sessTV view = do
  sess <- executeTransition (removePlayer (PlayerName pName)) sessTV
  showPlayers sess view

updatePlayerU :: FIBSMessage -> ModelAndViewUpdate
updatePlayerU wi sessTV view = do 
  let playerInfo = whoInfoToPlayerInfo wi
  sess <- executeTransition (updatePlayer playerInfo) sessTV
  -- TODO: this should not be called if executeTransition resulted in an error
  showPlayers sess view
  
whoInfoToPlayerInfo :: FIBSMessage -> PlayerInfo
whoInfoToPlayerInfo wi =
  PlayerInfo (PlayerName (Msg.name wi)) (Msg.ready wi) (gameState wi) (Msg.rating wi) (Msg.experience wi)
  where 
    gameState wi = case Msg.opponent wi of
      Just p -> Playing p
      Nothing -> case Msg.watching wi of
        Just p -> Watching p
        Nothing -> None
  
-- ** Misc ModelAndViewUpdates

-- | Displays the errors stored in SessionState in the View and removes them from SessionState.
reportErrors :: ModelAndViewUpdate
reportErrors sessTV view = do
  sess <- atomically $ takeTMVar sessTV
  let e = errors sess
  atomically $ putTMVar sessTV (sess `withErrors` [])
  showErrorMessages e view

-- * Binding view actions to model and view updates

controller :: SessionState -> View -> IO ()
controller session view = do
  sessionTV <- newTMVarIO session
  bindViewActions sessionTV view
  
bindViewActions :: TMVar SessionState -> View -> IO ()
bindViewActions sessionTV view = do
  setCommandHandler (logInItem $ sessionMenu view) (run loginU)
  setCommandHandler (logOutItem $ sessionMenu view) (run logoutU)  
  setCommandHandler (readyItem $ sessionMenu view) (run toggleReadyU)
  setCommandHandler (exitItem $ sessionMenu view) (run exitU)
  where
    run cmd = do forkIO $ cmd sessionTV view; return ()
      
-- * Helper functions

popErrorTV :: TMVar SessionState -> IO (Maybe String)
popErrorTV sessTV = do
  sess <- atomically $ takeTMVar sessTV
  let (err, sess') = popError sess
  atomically $ putTMVar sessTV sess'
  return err
  