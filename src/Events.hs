{-# LANGUAGE MultiParamTypeClasses #-}
{-|

-}
module Events ( Event (..)
              , EventConsumer (..)
              , consume
              , continue
              , terminate
              , EventQueueReader
              , EventQueueWriter
              , newEventQueue
              , getEvent
              , putEvent
              )
where
-- TChan for event queue
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
-- logging
import System.Log.Logger (debugM)

import DomainTypes

data Event = AddEventConsumer String EventConsumer
           | Disconnected
           | Error String
           | InviteRequest String String
           | LoginFailed String
           | LoginRequest String String
           | LoginSuccesful String
           | ToggleReadyRequest
           | PlayerUpdated PlayerInfo
           | PlayerRemoved String
           | ReadyOn
           | ReadyOff
           | RegistrationFailed String
           | RegistrationRequest String String
           | RegistrationSuccesful
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
  return $ Nothing

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

