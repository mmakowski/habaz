module Main where
import Model (initialSession)
import Events (newEventQueue)
import FIBSConnector (fibsConnector)
import View (createView)
import Dispatch (dispatchEvents)

import Graphics.UI.WX (start)

import Control.Concurrent (forkIO) 
-- TMVar for session state
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TMVar (newTMVar)

main :: IO ()
main = start $ do
  eventQueue <- newEventQueue
  view <- createView eventQueue
  forkIO $ dispatchEvents initialSession view (fibsConnector eventQueue) eventQueue 
