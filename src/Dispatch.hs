module Dispatch (
  dispatchEvents
) where
import Events (EventQueue)
import Model (Session)
import FIBSConnector (FIBSConnector)
import View (View)

dispatchEvents :: Session -> View -> FIBSConnector -> EventQueue -> IO ()
dispatchEvents _ _ _ _ = error "TODO" 