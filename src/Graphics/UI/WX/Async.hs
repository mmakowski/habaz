{-|
Module    : Graphics.UI.WX.Async
Copyright : (c) Maciek Makowski 2012
License   : BSD-style

Asynchronous processing for wxHaskell.
-}
module Graphics.UI.WX.Async 
  (
  -- * Types
    UpdateQueue
  , AsyncConfig (..)
  -- * Constants
  , defaultConfig
  -- * Functions
  , mkUpdateQueue
  , mkUpdateQueueWithConfig
  , postGUIAsync  
  ) where

-- TChan for update queue
import Control.Concurrent.STM (atomically, STM) 
import Control.Concurrent.STM.TChan (TChan, newTChanIO, isEmptyTChan, readTChan, writeTChan)

import Control.Monad (forM, forM_)

import Data.Maybe (catMaybes)

import Graphics.UI.WX

-- | They type of queue that delivers updates to the UI thread
type UpdateQueue = TChan (IO ())

-- | The configuration of the asynchronous processor
data AsyncConfig = AsyncConfig { pollIntervalMs :: Int
                               , batchSize      :: Int
                               }

defaultConfig :: AsyncConfig
defaultConfig = AsyncConfig { pollIntervalMs = 10
                            , batchSize      = 100
                            }

mkUpdateQueue :: Frame a -> IO UpdateQueue
mkUpdateQueue = mkUpdateQueueWithConfig defaultConfig

mkUpdateQueueWithConfig :: AsyncConfig -> Frame a -> IO UpdateQueue
mkUpdateQueueWithConfig cfg f = do
  q <- newTChanIO
  timer f [ interval   := pollIntervalMs cfg
          , on command := processUiUpdates (batchSize cfg) q
          ]
  return q

postGUIAsync :: UpdateQueue -> IO a -> IO ()
postGUIAsync q u = atomically $ writeTChan q $ do u; return ()

processUiUpdates :: Int -> UpdateQueue -> IO ()
processUiUpdates n q = atomically (tryTake n q) >>= sequence_

tryTake :: Int -> TChan a -> STM [a]
tryTake n q = forM [1..n] (\_ -> tryReadTChan q) >>= return . catMaybes

-- in Control.Concurrent.STM.TChan 2.4
tryReadTChan :: TChan a -> STM (Maybe a)
tryReadTChan c = do
  empty <- isEmptyTChan c
  if empty then return Nothing else readTChan c >>= return . Just
