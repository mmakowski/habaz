module Main where
import Model (initialSession)
import Events (newEventQueue)
import FIBSConnector (fibsConnector)
import View (createView)
import Dispatch (dispatchEvents)

import Graphics.UI.WX (start)
import Control.Concurrent (forkIO) 

-- logging
import System.IO (openFile, IOMode (..))
import System.Log.Logger
import System.Log.Handler.Simple (verboseStreamHandler)

main :: IO ()
main = start $ do
  setUpLogging
  eventQueue <- newEventQueue
  view <- createView eventQueue
  forkIO $ dispatchEvents initialSession view (fibsConnector eventQueue) eventQueue 

setUpLogging :: IO ()
setUpLogging = do
  logFile <- openFile "habaz.log" AppendMode
  logFileHandler <- verboseStreamHandler logFile DEBUG
  updateGlobalLogger rootLoggerName $ setHandlers [logFileHandler]
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  noticeM "Habaź" "Habaź starting"
