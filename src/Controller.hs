{-
-}
module Controller(
  controller
) where
import Model
import View
import FIBSClient (defaultFIBSHost, defaultFIBSPort)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)

-- * Composing Model transitions and View updates

type ModelAndViewUpdate = TMVar SessionState -> View -> IO ()

type StateDependentViewUpdate = SessionState -> ViewUpdate

(<>) :: SessionStateTransition -> StateDependentViewUpdate -> ModelAndViewUpdate
(<>) sessTrans viewUpd sessTV view = do
  sess <- atomically $ takeTMVar sessTV
  sess' <- sessTrans sess
  atomically $ putTMVar sessTV sess'
  viewUpd sess' view

-- ** Composed updates
  
loginCmd :: ModelAndViewUpdate
loginCmd = (login defaultFIBSHost defaultFIBSPort "habaztest_a" "habaztest") 
           <> (\s -> disableLogIn |> enableLogOut |> showInfoMessage (show s))

logoutCmd :: ModelAndViewUpdate
logoutCmd = logout 
            <> (\s -> enableLogIn |> disableLogOut |> showInfoMessage (show s))

exitCmd :: ModelAndViewUpdate
exitCmd _ = closeMainWindow

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
      
