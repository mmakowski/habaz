module FIBSConnector ( FIBSConnector
                     , (</)
                     , fibsConnector
                     ) 
where

-- FIBS message processing thread" 
import Control.Concurrent (forkIO)
-- looping
import Control.Monad (forM_)

import FIBSClient
import Events (putEvent, Event, EventQueue)
import qualified Events as E (Event (..))

data FIBSConnector = FIBSConnector (Event -> IO FIBSConnector)

-- | performs connector transition under supplied event
(</) :: FIBSConnector -> Event -> IO FIBSConnector
(FIBSConnector f) </ e = f e

-- | a connector that handles login and register events
fibsConnector :: EventQueue -> FIBSConnector
fibsConnector q = FIBSConnector $ loginOrRegisterTransition q

loginOrRegisterTransition :: EventQueue -> Event -> IO FIBSConnector
loginOrRegisterTransition q e = case e of
  E.LoginRequest user pass        -> loginConnector q user pass
  E.RegistrationRequest user pass -> registrationConnector q user pass
  _                               -> return $ fibsConnector q

-- | a connector that handles logging in
loginConnector :: EventQueue -> String -> String -> IO FIBSConnector
loginConnector q user pass = do
  conn <- connect defaultFIBSHost defaultFIBSPort
  loginStatus <- login conn "Habaź_v0.1.0" user pass
  case loginStatus of
    LoginFailure msg -> do putEvent q $ E.LoginFailed msg
                           disconnect conn
                           return $ fibsConnector q
    LoginSuccess     -> startProcessingMessages q conn

startProcessingMessages :: EventQueue -> ReadWriteConnection -> IO FIBSConnector
startProcessingMessages q rwconn = do
  (msgs, woconn) <- readMessages rwconn
  forkIO $ messageProcessor q msgs
  return $ commandSender q woconn

-- | a connector that translates events to FIBS commands and sends them to the server
commandSender :: EventQueue -> WriteOnlyConnection -> FIBSConnector
commandSender q conn = FIBSConnector $ commandSendingTransition q conn

commandSendingTransition :: EventQueue -> WriteOnlyConnection -> Event -> IO FIBSConnector
commandSendingTransition q conn e = do 
  forM_ (commandsFor e) $ sendCommand conn
  return $ commandSender q conn

commandsFor :: Event -> [FIBSCommand]
commandsFor E.ReadyOn  = [Toggle Ready]
commandsFor E.ReadyOff = [Toggle Ready]
commandsFor _          = []

messageProcessor :: EventQueue -> [ParseResult FIBSMessage] -> IO ()
messageProcessor q (msg:msgs) = do
  -- TODO: translate msg and put on q
  print msg
  forM_ (eventsFor msg) $ putEvent q
  messageProcessor q msgs

eventsFor :: ParseResult FIBSMessage -> [Event]
eventsFor (ParseSuccess msg) = eventsFor' msg
eventsFor (ParseFailure msg) = [E.Error msg]

eventsFor' :: FIBSMessage -> [Event]
eventsFor' (Welcome name _ _) = [E.LoginSuccesful name]
eventsFor' _                  = []

registrationConnector :: EventQueue -> String -> String -> IO FIBSConnector
registrationConnector q user pass = error "TODO"