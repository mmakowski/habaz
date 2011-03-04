{-
-}
module Controller(
  controller
) where
import Model
import View
import FIBSClient (defaultFIBSHost, defaultFIBSPort)
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

-- ** Simple composed updates
{-
loginCmd :: ModelAndViewUpdate
loginCmd = (login defaultFIBSHost defaultFIBSPort "habaztest_a" "habaztest") 
           <> (\s -> disableLogIn |> enableLogOut |> showInfoMessage (show s))
-}
logoutCmd :: ModelAndViewUpdate
logoutCmd = logout <> (\s -> enableLogIn |> disableLogOut |> showInfoMessage (show s))

exitCmd :: ModelAndViewUpdate
exitCmd _ = closeMainWindow

-- ** Complex commands

loginCmd :: ModelAndViewUpdate
loginCmd sessTV view = do
  disableLogIn view
  sess' <- executeTransition (login defaultFIBSHost defaultFIBSPort "habaztest_a" "habaztest") sessTV
  case sess' of
    (LoggedOut e)    -> do reportErrors sessTV view
                           enableLogIn view
    (LoggedIn c m e) -> do startMessageProcessingThread m
                           (disableLogIn |> enableLogOut) view
  where
    startMessageProcessingThread msgs = do 
      executeTransition startProcessingMessages sessTV
      -- TODO: stop this thread once current session is disconnected
      forkIO $ processMessage msgs
    processMessage (msg:msgs) = do
      putStrLn (show msg) -- TODO: state transition
      processMessage msgs
      
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
  setCommandHandler (logInItem $ menu view) (run loginCmd)
  setCommandHandler (logOutItem $ menu view) (run logoutCmd)  
  setCommandHandler (exitItem $ menu view) (run exitCmd)
  where
    run cmd = do forkIO $ cmd sessionTV view; return ()
      
