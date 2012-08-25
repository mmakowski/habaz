{-# LANGUAGE RecordWildCards #-}
{-
Module responsible for interfacing between the event queue and FIBS client
-}
module FIBSConnector ( fibsConnector ) 
where

-- FIBS message processing thread" 
import Control.Concurrent (forkIO)
-- looping
import Control.Monad (forM_, unless)
import Data.Maybe (isNothing)
-- logging
import System.Log.Logger (debugM)


import FIBSClient
import Backgammon
import DomainTypes hiding (name)
import Events (putEvent, Event, EventQueueWriter, EventConsumer (..), continue, terminate)
import qualified Events as E (Event (..))

-- | a connector that handles login and register events
fibsConnector :: EventQueueWriter -> EventConsumer
fibsConnector q = EventConsumer $ loginOrRegisterTransition q

loginOrRegisterTransition :: EventQueueWriter -> Event -> IO (Maybe EventConsumer)
loginOrRegisterTransition q e = case e of
  E.LoginRequest user pass        -> loginConnector q user pass
  E.RegistrationRequest user pass -> registrationConnector q user pass
  _                               -> continue $ fibsConnector q

-- | a connector that handles logging in
loginConnector :: EventQueueWriter -> String -> String -> IO (Maybe EventConsumer)
loginConnector q user pass = do
  conn <- connect defaultFIBSHost defaultFIBSPort
  loginStatus <- login conn "Habaź_v0.1.0" user pass
  case loginStatus of
    LoginFailure msg -> do putEvent q $ E.LoginFailed msg
                           disconnect conn
                           continue $ fibsConnector q
    LoginSuccess     -> startProcessingMessages q conn

startProcessingMessages :: EventQueueWriter -> ReadWriteConnection -> IO (Maybe EventConsumer)
startProcessingMessages q rwconn = do
  (msgs, woconn) <- readMessages rwconn
  forkIO $ messageProcessor q msgs
  commandSender q woconn 

-- | a connector that translates events to FIBS commands and sends them to the server
commandSender :: EventQueueWriter -> WriteOnlyConnection -> IO (Maybe EventConsumer)
commandSender q conn = continue $ EventConsumer $ commandSendingTransition q conn

commandSendingTransition :: EventQueueWriter -> WriteOnlyConnection -> Event -> IO (Maybe EventConsumer)
commandSendingTransition q conn e = do 
  forM_ (commandsFor e) $ logAndSendCommand conn
  if e == E.Disconnected then continue $ fibsConnector q else commandSender q conn

logAndSendCommand :: WriteOnlyConnection -> FIBSCommand -> IO ()
logAndSendCommand conn cmd = do
  debugM "FIBS.command" (show cmd)
  sendCommand conn cmd

registrationConnector :: EventQueueWriter -> String -> String -> IO (Maybe EventConsumer)
registrationConnector q user pass = do
  status <- createAccount defaultFIBSHost defaultFIBSPort user pass
  case status of
    AccountCreationSuccess     -> putEvent q E.RegistrationSuccesful
    AccountCreationFailure msg -> putEvent q $ E.RegistrationFailed msg
  continue $ fibsConnector q

messageProcessor :: EventQueueWriter -> [ParseResult FIBSMessage] -> IO ()
messageProcessor q (msg:msgs) = do
  debugM "FIBS.message" (show msg)
  forM_ (eventsFor msg) $ putEvent q
  unless (isTerminal msg) $ messageProcessor q msgs
messageProcessor q [] = do
  debugM "FIBS" "message stream exhausted"
  return ()


isTerminal :: ParseResult FIBSMessage -> Bool
isTerminal (ParseSuccess ConnectionTimeOut) = True
isTerminal _                                = False

-- translation between FIBS messages/commands and events

commandsFor :: Event -> [FIBSCommand]
commandsFor E.ToggleReadyRequest  = [ Toggle Ready ]
commandsFor (E.InviteRequest u l) = [ Invite u l ]
commandsFor _                     = []

eventsFor :: ParseResult FIBSMessage -> [Event]
eventsFor (ParseSuccess msg) = eventsFor' msg
eventsFor (ParseFailure msg) = [E.Error msg]

eventsFor' :: FIBSMessage -> [Event]
eventsFor' ConnectionTimeOut  = [ E.Disconnected ]
eventsFor' (Invitation n l)   = [ E.Invitation n l ]
eventsFor' (Logout name _)    = [ E.PlayerRemoved name ]
eventsFor' (OwnInfo {..})     = [ E.LoginSuccesful $ name
                                , if ready then E.ReadyOn else E.ReadyOff
                                , E.PlayerUpdated $ PlayerInfo name rating experience ready
                                ]
eventsFor' ReadyOn            = [ E.ReadyOn ]
eventsFor' ReadyOff           = [ E.ReadyOff ]
eventsFor' (System msg)       = [ E.Info msg ]
eventsFor' (WhoInfo {..})     = [ E.PlayerUpdated $ PlayerInfo name rating experience (ready && isNothing opponent) ]
eventsFor' _                  = []
