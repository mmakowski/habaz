{-
Module responsible for interfacing between the event queue and FIBS client
-}
module FIBSConnector ( fibsConnector ) 
where

-- FIBS message processing thread" 
import Control.Concurrent (forkIO)
-- looping
import Control.Monad (forM_)
-- logging
import System.Log.Logger (debugM)

import FIBSClient
import DomainTypes
import Events (putEvent, Event, EventQueueWriter, EventConsumer (..), continue)
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
  commandSender q conn

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
  messageProcessor q msgs

-- translation between FIBS messages/commands and events

commandsFor :: Event -> [FIBSCommand]
commandsFor E.ToggleReadyRequest = [Toggle Ready]
commandsFor _                    = []

eventsFor :: ParseResult FIBSMessage -> [Event]
eventsFor (ParseSuccess msg) = eventsFor' msg
eventsFor (ParseFailure msg) = [E.Error msg]

eventsFor' :: FIBSMessage -> [Event]
eventsFor' (Logout name _)    = [ E.PlayerRemoved name ]
eventsFor' (OwnInfo name _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ready _ _ _ _) = 
                                [ E.LoginSuccesful name
                                , if ready then E.ReadyOn else E.ReadyOff
                                ]
eventsFor' ReadyOn            = [ E.ReadyOn ]
eventsFor' ReadyOff           = [ E.ReadyOff ]
eventsFor' (Welcome name _ _) = [ E.LoginSuccesful name ]
eventsFor' (WhoInfo name opp _ ready _ rating exp _ _ _ _ _) =
                                [ E.PlayerUpdated (PlayerInfo name rating exp (ready && (opp == Nothing))) ]
eventsFor' _                  = []

