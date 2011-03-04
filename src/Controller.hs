{-
-}
module Controller(
  controller
) where
import Model
import View
import FIBSClient (defaultFIBSHost, defaultFIBSPort)
-- for mapping messages to updates
import FIBSClient.Messages
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

type StateDependentViewUpdate = SessionState -> ViewUpdate

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
logoutU = logout <> (\s -> disableLogOut |> showInfoMessage (show s))

exitU :: ModelAndViewUpdate
exitU _ = closeMainWindow

disconnectU :: ModelAndViewUpdate
disconnectU = disconnect <> (\s -> enableLogIn |> disableLogOut)

noOpU :: ModelAndViewUpdate
noOpU _ _ = return ()

-- ** Complex commands

loginU :: ModelAndViewUpdate
loginU sessTV view = do
  disableLogIn view
  sess' <- executeTransition (login defaultFIBSHost defaultFIBSPort "habaztest_a" "habaztest") sessTV
  case sess' of
    (Disconnected e) -> do reportErrors sessTV view
                           enableLogIn view
    (LoggedIn c m e) -> do startMessageProcessingThread m
                           (disableLogIn |> enableLogOut) view
  where
    startMessageProcessingThread msgs = do 
      executeTransition startProcessingMessages sessTV
      forkIO $ processMessage msgs
    processMessage (msg:msgs) = do
      putStrLn (show msg) -- TODO: state transition
      (updateForMessage msg) sessTV view
      if isTerminating msg then disconnectU sessTV view
        else processMessage msgs
      
-- ** Model and View Updates for FIBS messages

-- | Yields ModelAndViewUpdate corresponding to given FIBSMessage
updateForMessage :: ParseResult FIBSMessage -> ModelAndViewUpdate
updateForMessage (ParseFailure err) = 
  \sessTV view -> do executeTransition (logErrorIO err) sessTV
                     reportErrors sessTV view
updateForMessage (ParseSuccess msg) = case msg of
  OwnInfo _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> recogniseReadyU msg
  _ -> noOpU
  
recogniseReadyU :: FIBSMessage -> ModelAndViewUpdate
recogniseReadyU msg sessTV view = execAndShow $ if ready msg then recogniseReady else recogniseNotReady
  where
    execAndShow trans = do
      sess <- executeTransition trans sessTV
      showInfoMessage (show sess) view


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
  setCommandHandler (logInItem $ menu view) (run loginU)
  setCommandHandler (logOutItem $ menu view) (run logoutU)  
  setCommandHandler (exitItem $ menu view) (run exitU)
  where
    run cmd = do forkIO $ cmd sessionTV view; return ()
      
