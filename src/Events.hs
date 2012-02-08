{-|

-}
module Events ( Event (..)
              , EventQueue
              , newEventQueue
              , getEvent
              , putEvent
              )
where
-- TChan for event queue
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TChan (TChan, newTChan, writeTChan, readTChan)

data Event = RegistrationRequest String String
           | LoginRequest String String
           | LoginFailed String
           | LoginSuccesful String
           | ReadyOnRequest
           | ReadyOffRequest
           | ReadyOn
           | ReadyOff
           | Disconnected
           | Error String
  deriving (Eq, Show)

type EventQueue = TChan Event

newEventQueue :: IO EventQueue
newEventQueue = atomically $ newTChan

getEvent :: EventQueue -> IO Event
getEvent q = atomically $ readTChan q

putEvent :: EventQueue -> Event -> IO ()
putEvent q e = atomically $ writeTChan q e

