{-# LANGUAGE MultiParamTypeClasses #-}
{-|

-}
module Events ( Event (..)
              , EventQueueReader
              , EventQueueWriter
              , EventConsumer (..)
              , consume
              , newEventQueue
              , getEvent
              , putEvent
              )
where
-- TChan for event queue
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)

import DomainTypes

data Event = Disconnected
           | Error String
           | LoginRequest String String
           | LoginFailed String
           | LoginSuccesful String
           | ToggleReadyRequest
           | PlayerUpdated PlayerInfo
           | PlayerRemoved String
           | ReadyOn
           | ReadyOff
           | RegistrationRequest String String
  deriving (Eq, Show)


-- | event consumer consumes an event yielding a new consumer (potentially) and some IO actions
data EventConsumer = EventConsumer (Event -> IO (Maybe EventConsumer))

consume :: Event -> EventConsumer -> IO (Maybe EventConsumer)
consume e (EventConsumer c) = c e

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

