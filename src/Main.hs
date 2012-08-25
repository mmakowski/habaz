module Main where
--import Model (sessionConsumer) TODO: how do we combine session state with other consumers?
import Events (newEventQueue)
import FIBSConnector (fibsConnector)
import Model (newInitialState, stateUpdater)
import View (viewConsumer)
import Dispatch (dispatchEvents)

import Graphics.UI.WX (start)
import Control.Concurrent (forkIO) 

-- logging
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger

main :: IO ()
main = start $ do
  setUpLogging
  (qreader, qwriter) <- newEventQueue
  state <- newInitialState
  consumers <- sequence [ return $ stateUpdater state
                        , viewConsumer qwriter state
                        , return $ fibsConnector qwriter
                        ]
  forkIO $ dispatchEvents consumers qreader

setUpLogging :: IO ()
setUpLogging = do
  logFileHandler <- fileHandler "habaz.log" DEBUG >>= \lh -> return $
                 setFormatter lh (simpleLogFormatter "$time $loggername [$tid] $prio $msg")
  updateGlobalLogger rootLoggerName $ setHandlers [logFileHandler]
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  noticeM "Habaź" "Habaź starting"
