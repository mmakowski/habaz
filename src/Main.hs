module Main where
import Model (initialSession)
import Events (newEventQueue)
import FIBSConnector (fibsConnector)
import View (createView)
import Dispatch (dispatchEvents)

import Graphics.UI.WX (start)
-- TMVar for session state
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TMVar (newTMVar)

main :: IO ()
main = start $ do
  eventQueue <- newEventQueue
  view <- createView
  dispatchEvents initialSession view (fibsConnector eventQueue) eventQueue 

