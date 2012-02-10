module Main where
--import Model (sessionConsumer) TODO: how do we combine session state with other consumers?
import Events (newEventQueue)
import FIBSConnector (fibsConnector)
import View (createView, viewConsumer)
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
  (qreader, qwriter) <- newEventQueue
  view <- createView qwriter
  consumers <- sequence [ viewConsumer view qwriter
  						, return $ fibsConnector qwriter
                        ]
  forkIO $ dispatchEvents consumers qreader

setUpLogging :: IO ()
setUpLogging = do
  logFile <- openFile "habaz.log" AppendMode
  logFileHandler <- verboseStreamHandler logFile DEBUG
  updateGlobalLogger rootLoggerName $ setHandlers [logFileHandler]
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  noticeM "Habaź" "Habaź starting"
