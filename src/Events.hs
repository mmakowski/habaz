{-|

-}
module Events ( Event (..)
              , EventQueue
              , newEventQueue
              )
where
-- TChan for event queue
import Control.Concurrent.STM (atomically) 
import Control.Concurrent.STM.TChan (TChan, newTChan)

data Event = RegistrationRequest String String
           | LoginRequest String String
           | LoginFailed
           | LoginSuccesful String
           | ReadyOnRequest
           | ReadyOffRequest
           | ReadyOn
           | ReadyOff
           | Disconnected

type EventQueue = TChan Event

newEventQueue :: IO EventQueue
newEventQueue = atomically $ newTChan
