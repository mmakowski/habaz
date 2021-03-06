import Data.Maybe (catMaybes)

-- TChan for event queue
import Control.Concurrent.STM (atomically, STM) 
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import Control.Concurrent (forkIO) 
import Control.Monad (forever, forM, forM_, when)
import Data.Maybe (catMaybes)

-- logging
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Logger


-- wx
import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)

-- gtk
--import Graphics.UI.Gtk

-- Graphics.UI.WX.Async

data UpdateQueue a = UpdateQueue { uqHandler :: Frame a
                                 , uqQueue   :: TQueue (IO ())
                                 , uqConfig  :: AsyncConfig
                                 }

data AsyncConfig = AsyncConfig { eventId   :: Id
                               , batchSize :: Int
                               }

defaultConfig :: AsyncConfig
defaultConfig = AsyncConfig { eventId   = wxID_HIGHEST + 51
                            , batchSize = 100
                            }

mkUpdateQueue :: Frame a -> IO (UpdateQueue a)
mkUpdateQueue = mkUpdateQueueWithConfig defaultConfig

mkUpdateQueueWithConfig :: AsyncConfig -> Frame a -> IO (UpdateQueue a)
mkUpdateQueueWithConfig cfg f = do
  q <- newTQueueIO
  evtHandlerOnMenuCommand f (eventId cfg) $ processUiUpdates (batchSize cfg) q
  return $ UpdateQueue f q cfg

processUiUpdates :: Int -> TQueue (IO ()) -> IO ()
processUiUpdates n q = atomically (tryTake n q) >>= sequence_

tryTake :: Int -> TQueue a -> STM [a]
tryTake n q = forM [1..n] (\_ -> tryReadTQueue q) >>= return . catMaybes

postGUIAsync :: UpdateQueue a -> IO b -> IO ()
postGUIAsync q u = do 
  atomically $ writeTQueue (uqQueue q) $ do u; return ()
  mkEvent (eventId $ uqConfig q) >>= evtHandlerAddPendingEvent (uqHandler q)

mkEvent :: Id -> IO (CommandEvent ())
mkEvent eventId = commandEventCreate wxEVT_COMMAND_MENU_SELECTED eventId

-- Events ---------------------------------------------------------------------------

data Event = Test Int
  deriving (Eq, Show)


-- | event consumer consumes an event yielding a new consumer (potentially) and some IO actions
data EventConsumer = EventConsumer (Event -> IO (Maybe EventConsumer))
instance Show EventConsumer where show _ = "<EventConsumer>"
instance Eq EventConsumer where _ == _ = False

consume :: Event -> EventConsumer -> IO (Maybe EventConsumer)
consume e (EventConsumer c) = c e

continue :: EventConsumer -> IO (Maybe EventConsumer)
continue c = return $ Just c

terminate :: String -> IO (Maybe EventConsumer)
terminate name = do
  debugM "Habaź.consumer" $ "terminating consumer " ++ name
  return Nothing

-- | event queue is a transactional channel of Events
type EventQueue = TChan Event

-- | the reader end of EventQueue
data EventQueueReader = EventQueueReader EventQueue
-- | the writer end of EventQueue
data EventQueueWriter = EventQueueWriter EventQueue

newEventQueue :: IO (EventQueueReader, EventQueueWriter)
newEventQueue = do
  q <- newTChanIO
  return (EventQueueReader q, EventQueueWriter q)

getEvent :: EventQueueReader -> IO Event
getEvent (EventQueueReader q) = atomically $ readTChan q

putEvent :: EventQueueWriter -> Event -> IO ()
putEvent (EventQueueWriter q) e = atomically $ writeTChan q e


-- Dispatch ------------------------------------------------------------------------

-- | contiuously processes events from the queue and dispatches them to all consumers
dispatchEvents :: [EventConsumer] -> EventQueueReader -> IO ()
dispatchEvents ecs q = do
  e <- getEvent q
  debugM "event" (show e)
  ecs' <- mapM (consume e) ecs
  dispatchEvents (catMaybes ecs') q

-- View ----------------------------------------------------------------------------

viewConsumer :: EventQueueWriter -> EventConsumer
viewConsumer q = EventConsumer $ \e -> do
  debugM "view" (show e)
  continue $ viewConsumer q

-- wx
withView :: ((EventQueueWriter -> IO EventConsumer) -> IO a) -> IO ()
withView prog = start $ prog viewConsumerIO

viewConsumerIO q = do
  f <- frame [ text := "Scratch" ]
  mkUpdateQueue f
  return $ viewConsumer q

-- gtk

--withView :: ((EventQueueWriter -> IO EventConsumer) -> IO a) -> IO ()
--withView prog = do
--  initGUI
--  prog viewConsumerIO
--  mainGUI

--viewConsumerIO :: EventQueueWriter -> IO EventConsumer
--viewConsumerIO q = do
--  f <- windowNew
--  onDestroy f mainQuit
--  widgetShowAll f
--  return $ viewConsumer q


-- FibsConnector -------------------------------------------------------------------

fibsConnector :: EventQueueWriter -> IO EventConsumer
fibsConnector q = do
  forkIO $ messageGenerator q
  return $ EventConsumer $ \_ -> terminate "FIBS connector"

messageGenerator :: EventQueueWriter -> IO ()
messageGenerator q = forM_ [1..10000] $ \i -> do
  let e = Test i
  debugM "gen" (show e)
  putEvent q e

-- Main ----------------------------------------------------------------------------

main :: IO ()
main = withView $ \viewConsumerIO -> do
  setUpLogging
  (qreader, qwriter) <- newEventQueue
  consumers <- sequence [ viewConsumerIO qwriter
                        , fibsConnector qwriter
                        ]
  forkIO $ dispatchEvents consumers qreader

setUpLogging :: IO ()
setUpLogging = do
  logFileHandler <- fileHandler "scratch.log" DEBUG >>= \lh -> return $
                 setFormatter lh (simpleLogFormatter "$time $loggername [$tid] $prio $msg")
  updateGlobalLogger rootLoggerName $ setHandlers [logFileHandler]
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  noticeM "Scratch" "Scratch starting"
