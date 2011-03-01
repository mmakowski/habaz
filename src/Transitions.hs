{-|
This module defines functions that transition between session, match and game states.
-}
module Transitions(
  login,
  transition
) where
import FIBSClient hiding (login, logout, Flag (..))
import qualified FIBSClient (login, logout, Flag (..))
import FIBSClient.Messages (splitByFirst, splitByFirstWithLimit, isReadyOn, isOwnInfo, isWhoInfo)
import Session
import System.IO
import System.IO.Error
import Test.HUnit hiding (errors)
import TestUtils

-- | Type alias to make it clear where a function returns a state transition. Session state transitions
-- might involve IO actions hence the result type is tainted with IO.
type SessionStateTransition = SessionState -> IO SessionState


-- | Tries to connect and log in to FIBS.
login :: String                  -- ^ host
      -> String                  -- ^ port
      -> String                  -- ^ user name
      -> String                  -- ^ password
      -> SessionStateTransition  -- ^ the state transition
login host port userName password s@(LoggedOut es) = connectAndLogin `catch` errorHandler
  where
    connectAndLogin = 
      do conn <- connect host port
         loginStatus <- FIBSClient.login conn "Habaź_v0.1.0" userName password
         case loginStatus of
           LoginFailure e -> do disconnect conn
                                logErrorIO e s
           LoginSuccess   -> do return $ LoggedIn conn es
    errorHandler e = logErrorIO ("error connecting to " ++ host ++ ":" ++ port) s
login _ _ _ _ s = logErrorIO ("unable to login in " ++ (stateName s) ++ " state") s

slowtest_loginUnsuccesfulConnectLogsError =
  do s <- login "wronghost" "1234" "u" "p" initialSessionState
     assertEqual "" (initialSessionState `withErrors` ["error connecting to wronghost:1234"]) s
test_loginWrongStateLogsError = 
  do conn <- dummyConnection
     let s = (NotReady conn [])
     s' <- testLogin s
     assertEqual "" ["unable to login in NotReady state"] (errors s')
slowtest_loginSuccessTLoggedIn =
  do s' <- testLogin initialSessionState
     case s' of
       LoggedIn _ [] -> return () 
       _             -> assertFailure $ "unexpected state after login: " ++ (show s')


-- | Logs out and disconnects from FIBS.
logout :: SessionStateTransition
logout s@(LoggedOut _) = logErrorIO "unable to logout in LoggedOut state" s
logout s = 
  do let conn = connection s
     FIBSClient.logout conn
     disconnect conn
     return $ LoggedOut $ errors s

test_logoutWrongStateLogsError =
  do s <- logout initialSessionState
     assertEqual "" ["unable to logout in LoggedOut state"] (errors s)
slowtest_logoutPreservesErrors =   
  do s <- testLogin initialSessionState
     s' <- logout (s `withErrors` es)
     assertEqual "" (LoggedOut es) s'
  where es = ["error 1", "error 2"]
  

-- | Translates message parse result to session transition.
transition :: ParseResult FIBSMessage -> SessionStateTransition
transition (ParseSuccess m) = transFM m
transition (ParseFailure e) = logErrorIO e

test_transitionForParseFailureIsErrorLogged =
  do st <- transition (ParseFailure "parse error msg") initialSessionState
     assertEqual "" ["parse error msg"] (errors st)


-- | Translates given FIBSMessage to session transition
transFM :: FIBSMessage -> SessionStateTransition
transFM msg = case msg of 
  _ -> logErrorIO $ "unsupported message: " ++ (show msg)
  
ignoreMessage :: FIBSMessage -> SessionStateTransition
ignoreMessage _ = return

-- FailedLogin

-- FreeForm
freeForm = ignoreMessage

-- System
system = ignoreMessage

-- Welcome
  
-- OwnInfo: TODO

-- MOTD
motd = ignoreMessage

-- helper functions
logErrorIO :: String -> SessionStateTransition
logErrorIO e st = return $ logError e st

-- test data and helper functions

testAccount = "habaztest_a"
testPassword = "habaztest"
testLogin = login defaultFIBSHost defaultFIBSPort testAccount testPassword
sampleWelcomeMsg = Welcome "username" (toUTCTime "1231233") "host"
