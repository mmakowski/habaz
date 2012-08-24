{-|
Module    : Graphics.UI.WX.Async
Copyright : (c) Maciek Makowski 2012
License   : MIT

Asynchronous processing for wxHaskell. wxWidgets requires that all UI updates are invoked from
the same thread; this library provides a way of initiating the updates from arbitrary threads.
It is done by posting the UI update IO actions onto a transactional queue which is periodically
checked by the thread that runs the main UI loop.

Example usage:

>main = start $ do
>  f <- frame [ text := "Start" ]
>  q <- newUpdateQueue f
>  forkIO $ asyncUIUpdate q f
>
>-- now we can update the UI from another thread:
>asyncUIUpdate = postGUIAsync q $ set f [ text := "End" ]

There is normally no reasaon to create more than one update queue per application.

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

import Control.Monad (forM, forM_)

import Data.Maybe (catMaybes)

import Graphics.UI.WX

-- | They type of queue that delivers updates to the UI thread.
type UpdateQueue = TChan (IO ())

-- | The configuration of the asynchronous processor.
data AsyncConfig = AsyncConfig { pollIntervalMs :: Int -- ^ the period between checks if there are any updates on the queue
                               , batchSize      :: Int -- ^ the maximum number of updates applied during single poll
                               }

-- | The default configuraion of a new update queue.
defaultConfig :: AsyncConfig
defaultConfig = AsyncConfig { pollIntervalMs = 10
                            , batchSize      = 100
                            }

-- | Yields a new update queue using 'defaultConfig'.
newUpdateQueue :: Window a       -- ^ a window of the application; when deleted, the queue will become defunct
               -> IO UpdateQueue -- ^ a new update queue attached to supplied window
newUpdateQueue = newUpdateQueueWithConfig defaultConfig

-- | Yields a new update queue with specified config.
newUpdateQueueWithConfig :: AsyncConfig    -- ^ queue configuration
                         -> Window a       -- ^ a window of the application; when deleted, the queue will become defunct
                         -> IO UpdateQueue -- ^ a new update queue attached to supplied window
newUpdateQueueWithConfig cfg f = do
  q <- newTChanIO
  timer f [ interval   := pollIntervalMs cfg
          , on command := processUiUpdates (batchSize cfg) q
          ]
  return q

-- | Posts an UI update onto specified 'UpdateQueue'.
postGUIAsync :: UpdateQueue -- ^ the queue to post the update to
             -> IO a        -- ^ the update to post
             -> IO ()
postGUIAsync q u = atomically $ writeTChan q $ do u; return ()

processUiUpdates :: Int -> UpdateQueue -> IO ()
processUiUpdates n q = atomically (tryTake n q) >>= sequence_

tryTake :: Int -> TChan a -> STM [a]
tryTake n q = forM [1..n] (\_ -> tryReadTChan q) >>= return . catMaybes

-- in Control.Concurrent.STM.TChan since 2.4
tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan c = do
  empty <- isEmptyTChan c
  if empty then return Nothing else readTChan c >>= return . Just
