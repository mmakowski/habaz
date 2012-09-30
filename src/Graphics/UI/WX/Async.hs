{-|
Module    : Graphics.UI.WX.Async
Copyright : (c) Maciek Makowski 2012
License   : MIT

Asynchronous processing for wxHaskell. wxWidgets requires that all UI updates are invoked from
the same thread; this library provides a way of initiating the updates from arbitrary threads.
It is done by posting the UI update IO actions onto a transactional queue and notifying the main UI
loop by means of WX event that an update is available.

Example usage:

>main = start $ do
>  f <- frame [ text := "Start" ]
>  q <- newUpdateQueue f
>  forkIO $ asyncUIUpdate q f
>
>-- now we can update the UI from another thread:
>asyncUIUpdate = postGUIAsync q $ set f [ text := "End" ]

There is normally no reasaon to create more than one update queue per application. If you choose to
do so, you'll need to ensure that a different event id is used for each of the queues.
-}
module Graphics.UI.WX.Async 
  (
  -- * Types
    UpdateQueue
  , AsyncConfig (..)
  -- * Constants
  , defaultConfig
  -- * Functions
  , newUpdateQueue
  , newUpdateQueueWithConfig
  , postGUIAsync  
  ) where

-- TChan for update queue
import Control.Concurrent.STM (atomically, STM) 
import Control.Concurrent.STM.TChan (TChan, newTChanIO, isEmptyTChan, readTChan, writeTChan)

import Control.Monad (forM, liftM)

import Data.Maybe (catMaybes)

import Graphics.UI.WX
import Graphics.UI.WXCore

-- | They type of queue that delivers updates to the UI thread.
data UpdateQueue = UpdateQueue { uqHandler :: Frame ()
                               , uqQueue   :: TChan (IO ())
                               , uqConfig  :: AsyncConfig
                               }

-- | The configuration of the asynchronous processor.
data AsyncConfig = AsyncConfig { eventId   :: Int -- ^ the id of event used to signal that an async update has been posted
                               , batchSize :: Int -- ^ the maximum number of updates applied during single queue poll
                               }

-- | The default configuraion of a new update queue.
defaultConfig :: AsyncConfig
defaultConfig = AsyncConfig { eventId   = wxID_HIGHEST + 51
                            , batchSize = 100
                            }

-- | Yields a new update queue using 'defaultConfig'.
newUpdateQueue :: Frame ()       -- ^ a window of the application; when deleted, the queue will become defunct
               -> IO UpdateQueue -- ^ a new update queue attached to supplied window
newUpdateQueue = newUpdateQueueWithConfig defaultConfig

-- | Yields a new update queue with specified config.
newUpdateQueueWithConfig :: AsyncConfig    -- ^ queue configuration
                         -> Frame ()       -- ^ a window of the application; when deleted, the queue will become defunct
                         -> IO UpdateQueue -- ^ a new update queue attached to supplied window
newUpdateQueueWithConfig cfg f = do
  q <- newTChanIO
  evtHandlerOnMenuCommand f (eventId cfg) $ processUiUpdates (batchSize cfg) q
  return $ UpdateQueue f q cfg

-- | Posts an UI update onto specified 'UpdateQueue'.
postGUIAsync :: UpdateQueue -- ^ the queue to post the update to
             -> IO b        -- ^ the update to post
             -> IO ()
postGUIAsync q u = do
  atomically $ writeTChan (uqQueue q) $ do u; return ()
  mkEvent (eventId $ uqConfig q) >>= evtHandlerAddPendingEvent (uqHandler q)

processUiUpdates :: Int -> TChan (IO ()) -> IO ()
processUiUpdates n q = atomically (tryTake n q) >>= sequence_

tryTake :: Int -> TChan a -> STM [a]
tryTake n q = liftM catMaybes $ forM [1..n] $ \_ -> tryReadTChan q

mkEvent :: Id -> IO (CommandEvent ())
mkEvent eventId = commandEventCreate wxEVT_COMMAND_MENU_SELECTED eventId

-- in Control.Concurrent.STM.TChan since 2.4
tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan c = do
  empty <- isEmptyTChan c
  if empty then return Nothing else liftM Just $ readTChan c
