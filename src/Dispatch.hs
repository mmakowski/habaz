module Dispatch (
  dispatchEvents
) where
import Events (EventQueue, getEvent)
import Model (Session, (<|))
import FIBSConnector (FIBSConnector, (</))
import View (View)

dispatchEvents :: Session -> View -> FIBSConnector -> EventQueue -> IO ()
dispatchEvents s v f q = do
  e <- getEvent q
  print e -- TODO: debugging
  let s' = s <| e
  f' <- f </ e
  -- TODO: view update
  dispatchEvents s' v f' q
