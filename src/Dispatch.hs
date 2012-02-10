{-|
A central module in the application, handles dispatching events to all components.
-}
module Dispatch (
  dispatchEvents
) where
-- logging
import System.Log.Logger (debugM)
import Data.Maybe (catMaybes)

import Events (EventConsumer, EventQueueReader, consume, getEvent)

-- | contiuously processes events from the queue and dispatches them to all consumers
dispatchEvents :: [EventConsumer] -> EventQueueReader -> IO ()
dispatchEvents ecs q = do
  e <- getEvent q
  debugM "Habaź.event" (show e)
  ecs' <- mapM (consume e) ecs
  dispatchEvents (catMaybes ecs') q
