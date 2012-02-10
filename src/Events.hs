{-# LANGUAGE MultiParamTypeClasses #-}
{-|

-}
module Events ( Event (..)
              , EventQueue
              , EventConsumer
              , (<|)
              , newEventQueue
              , getEvent
              , putEvent
              )
where
-- TChan for event queue
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)

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

class EventConsumer a b where
  (<|) :: a     -- ^ initial state of consumer
       -> Event -- ^ the event that triggers transition
       -> b     -- ^ final state of consumer

type EventQueue = TChan Event

newEventQueue :: IO EventQueue
newEventQueue = atomically $ newTChan

getEvent :: EventQueue -> IO Event
getEvent q = atomically $ readTChan q

putEvent :: EventQueue -> Event -> IO ()
putEvent q e = atomically $ writeTChan q e

