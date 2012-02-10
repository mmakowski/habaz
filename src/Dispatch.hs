{-|
A central module in the application, handles dispatching events to all components.
-}
module Dispatch (
  dispatchEvents
) where
-- logging
import System.Log.Logger (debugM)

import Events (EventQueue, getEvent)
import Model (Session, (<|))
import FIBSConnector (FIBSConnector, (<|))
import View (View, (<|))

-- | contiuously processes events from the queue and dispatches them to all
-- components of the application.
dispatchEvents :: Session -> View -> FIBSConnector -> EventQueue -> IO ()
dispatchEvents s v f q = do
  e <- getEvent q
  debugM "Habaź.event" (show e)
  let s' = s <| e
  f' <- f <| e
  v' <- (v, q) <| e
  dispatchEvents s' v' f' q
