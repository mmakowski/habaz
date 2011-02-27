{-|
This module contains data types and functions modelling the states of FIBS session and transitions between them.
-}
module Session(
  -- * Types
  Session,
  SessionState (..),
  -- * Constants
  initialSession,
  -- * Functions
  transition
) where
import Backgammon
import FIBSClient.Messages
import System.Random
import Test.HUnit
import Test.QuickCheck
import TestUtils

data SessionState
     = NotLoggedIn
     | NotReady
     | Ready
     | Inviting
     | Invited
     | Playing
     deriving (Eq, Show)

sessionStates = [NotLoggedIn, NotReady, Ready, Inviting, Invited, Playing]

instance Arbitrary SessionState where
  arbitrary = elements sessionStates
  coarbitrary _ = id -- not needed

data Match = Match -- TODO: include board and stuff in there
           deriving (Eq, Show)

data Session 
     = Session { sessionState :: SessionState
               , match :: Maybe Match
               , errors :: [String]
               }
     deriving (Eq, Show)

instance Arbitrary Session where
  arbitrary = 
    do s <- arbitrary
       let m = Nothing -- TODO: arbitrary match
       e <- arbitrary
       return (Session s m e)
                 
  coarbitrary _ = id -- not needed

-- constants

-- | The session state at the start of the application
initialSession = Session NotLoggedIn Nothing []

-- session state transitions

-- | Translates message parse result to session transition
transition :: ParseResult FIBSMessage -> Session -> Session
transition (ParseSuccess m) = transFM m
transition (ParseFailure e) = logError e

test_transitionForParseResultIsErrorLogged =
  assertEqual ""
              (sampleSession `withErrors` ["parse error msg"])
              (transition (ParseFailure "parse error msg") (sampleSession `withErrors` []))

-- | Translates given FIBSMessage to session transition
transFM :: FIBSMessage -> Session -> Session
transFM msg = case msg of 
  FailedLogin     -> failedLogin
  m@(FreeForm _)  -> freeForm m 
  m@(System _)    -> system m
  (Welcome _ _ _) -> welcome
  -- TODO: own info
  m@(MOTD _)      -> motd m
  _ -> logError $ "unsupported message: " ++ (show msg)
  
ignoreMessage _ = id

-- FailedLogin
failedLogin s@(Session NotLoggedIn Nothing e) = logError "failed to log in" s
failedLogin s = logError "unexpected login prompt" s

test_failedLoginNotLoggedInLogsError = 
  assertEqual ""
              (initialSession `withErrors` ["failed to log in"])
              (transFM FailedLogin initialSession)
test_failedLoginInUnexpectedStateLogsError =
  assertEqual ""
              (loggedInSession `withErrors` ["unexpected login prompt"])
              (transFM FailedLogin loggedInSession)

-- FreeForm
freeForm = ignoreMessage

prop_freeFormIsIdentity m s = transFM (FreeForm m) s == s

-- System
system = ignoreMessage

prop_systemIsIdentity m s = transFM (System m) s == s

-- Welcome
welcome s@(Session NotLoggedIn Nothing e) = s `inState` NotReady
welcome s = logError "unexpected Welcome message" s
  
test_welcomeNotLoggedInNotReady =
  assertEqual ""
              (initialSession `inState` NotReady)
              (transFM sampleWelcomeMsg initialSession)
test_welcomeInUnexpectedStateLogsError = 
  assertEqual ""
              (loggedInSession `withErrors` ["unexpected Welcome message"])
              (transFM sampleWelcomeMsg loggedInSession)

-- OwnInfo: TODO

-- MOTD
motd = ignoreMessage

prop_motdIsIdentity m s = transFM (MOTD m) s == s

-- helper functions for session manipulation

(Session s m _) `withErrors` e = Session s m e
(Session _ m e) `inState` s = Session s m e

logError :: String -> Session -> Session
logError error (Session s m errors) = Session s m (errors ++ [error])

prop_logErrorAppendsToListOfErrors e s@(Session _ _ es) = logError e s == s `withErrors` (es ++ [e])

-- sample data for tests

sampleSession = initialSession
loggedInSession = sampleSession `inState` Ready

sampleWelcomeMsg = Welcome "username" (toUTCTime "1231233") "host"