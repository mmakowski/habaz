import Data.List
import Data.Maybe
import Data.Time
import FIBSClient
import System.Locale
import Test.HUnit
 
testAccount = "habaztest_a"
testPassword = "habaztest"

test_parseLine_failedLoginParsedCorrectly = assertEqual "failed login" FailedLogin $ parseLine "login:"
test_parseLine_welcomeParsedCorrectly = assertEqual "welcome message"
                                          (Welcome "username" (fromJust $ parseTime defaultTimeLocale "%s" "1041253132") "1.2.3.4") 
                                          (parseLine "1 username 1041253132 1.2.3.4")

-- Note: this test will normally be disabled because there is no way to remove 
-- a test account from FIBS once it was created. If other tests fail because
-- the test account does not exist then enable this test to re-create the account
test_createAccount = 
  do conn <- connect defaultFibsHost defaultFibsPort
     loginStatus <- login conn "habaz-test" testAccount testPassword
     result <- createTestAccount loginStatus conn
     disconnect conn
     return result
  where
    createTestAccount (Failure "invalid login details") _ = assertBool "TODO" False
    createTestAccount (Failure reason) _ = assertFailure reason
    createTestAccount Success conn = 
      do
        logout conn
        assertFailure $ testAccount ++ " is already a valid FIBS account, can't test creation"
    
{-
test_accountCreation = 
  do conn <- connect defaultFibsHost defaultFibsPort
     --readUntil "login:" conn
     send conn "guest\n"
     str <- readUntil "!!!" conn
     -- putStr str
     send conn "bye\n"
     disconnect conn
     return $ assertBool "not logged in as guest" $ isInfixOf "You just logged in as guest" str

-}
