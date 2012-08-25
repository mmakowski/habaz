{-|
This module contains data types and functions modeling the state of the application.
-}
module Model ( State
             , newInitialState
             , stateUpdater
             , playerInfo
             ) 
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)

import Control.Monad (liftM)

import qualified Data.Map as Map (Map, delete, empty, insert, lookup)

import Backgammon
import DomainTypes
import Events hiding (Disconnected)
import qualified Events as E (Event (Disconnected))

-- | State of the application (mutable)
newtype State = State (TVar StateData)

data StateData = StateData { sdPlayerInfo :: Map.Map String PlayerInfo 
                           }

-- | Creates a new initial state 
newInitialState :: IO State
newInitialState = liftM State $ newTVarIO initialStateData

initialStateData :: StateData
initialStateData = StateData { sdPlayerInfo = Map.empty }

-- | An event consumer that updates that state in response to events
stateUpdater :: State          -- ^ the state to update
             -> EventConsumer  -- ^ consumer that updates state in response to events
stateUpdater s = EventConsumer $ \e -> do
  updateState e s
  continue $ stateUpdater s

updateState :: Event -> State -> IO ()
updateState e (State tvsd) = atomically $ do
  sd <- readTVar tvsd
  writeTVar tvsd $ updateStateData e sd

updateStateData :: Event -> StateData -> StateData
updateStateData e sd = case e of 
  (PlayerUpdated pinfo) -> sd { sdPlayerInfo = Map.insert (name pinfo) pinfo $ sdPlayerInfo sd }
  (PlayerRemoved pname) -> sd { sdPlayerInfo = Map.delete pname $ sdPlayerInfo sd }
  _                     -> sd

-- | Yields the player info for player with supplied name
playerInfo :: State                 -- ^ the state
           -> String                -- ^ player name
           -> IO (Maybe PlayerInfo) -- ^ @Just@ player info, or @Nothing@ if no such player exists
playerInfo s n = do 
  pis <- extract sdPlayerInfo s
  return $ Map.lookup n pis

extract :: (StateData -> a) -> State -> IO a
extract e (State tvsd) = atomically $ liftM e $ readTVar tvsd
