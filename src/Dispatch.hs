{-|
A central module in the application, handles dispatching events to all components.
-}
module Dispatch (
  dispatchEvents
) where
import Events (EventQueue, getEvent)
import Model (Session, (<|))
import FIBSConnector (FIBSConnector, (<|))
import View (View, (<|))

-- | contiuously processes events from the queue and dispatches them to all
-- components of the application.
dispatchEvents :: Session -> View -> FIBSConnector -> EventQueue -> IO ()
dispatchEvents s v f q = do
  e <- getEvent q
  print e -- TODO: proper logging
  let s' = s <| e
  f' <- f <| e
  v' <- v <| e
  dispatchEvents s' v' f' q
