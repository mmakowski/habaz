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

instance Arbitrary Char where
  arbitrary = elements [' '..'z']
  coarbitrary _ = id -- not needed  

instance Arbitrary Session where
  arbitrary = elements [(Session s m e) |
                        s <- sessionStates,
                        m <- [Nothing], -- TODO: generate random matches
                        e <- generate 20 (System.Random.mkStdGen 0) arbitrary]
  coarbitrary _ = id -- not needed

-- constants

-- | The session state at the start of the application
initialSession = Session NotLoggedIn Nothing []

-- session state transitions

-- | Translates given FIBSMessage to session transition
transition :: FIBSMessage -> Session -> Session
transition msg = case msg of 
  (Welcome _ _ _) -> welcome
  _ -> logError $ "unsupported message: " ++ (show msg)
  
-- Welcome
welcome (Session NotLoggedIn Nothing e) = Session NotReady Nothing e
welcome s = logError "unexpected Welcome message" s
  
test_welcomeNotLoggedInNotReady =
  assertEqual ""
              (initialSession `inState` NotReady)
              (transition sampleWelcomeMsg initialSession)

test_welcomeInUnexpectedStateLogsError = 
  assertEqual ""
              (loggedInSession `withErrors` ["unexpected Welcome message"])
              (transition sampleWelcomeMsg loggedInSession)
  where
    loggedInSession = sampleSession `inState` Ready

-- helper functions

logError :: String -> Session -> Session
logError error (Session s m errors) = Session s m (errors ++ [error])

prop_logErrorAppendsToListOfErrors e s@(Session _ _ es) = logError e s == s `withErrors` (es ++ [e])

-- sample data for tests

sampleSession = initialSession                                      
(Session s m _) `withErrors` e = Session s m e
(Session _ m e) `inState` s = Session s m e

sampleWelcomeMsg = Welcome "username" (toUTCTime "1231233") "host"