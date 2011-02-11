import Data.List
import FIBSClient
import Test.HUnit
 
testAccount = "habaztest"
testPassword = "habaztest"

-- Note: this test will normally be disabled because there is no way to remove 
-- a test account from FIBS once it was created. If other tests fail because
-- the test account does not exist then enable this test to re-create the account
test_createAccount = 
  do conn <- connect defaultFibsHost defaultFibsPort
     loginStatus <- login conn "habaz-test" testAccount testPassword
     result <- tryToCreateAccount loginStatus
     disconnect conn
     return result
  where
    tryToCreateAccount (Failure "") = assertBool "TODO" False
    tryToCreateAccount (Failure msg) = assertFailure $ "unexpected problems when testing for account existence: " ++ msg
    tryToCreateAccount Success = assertFailure $ testAccount ++ " is already a valid FIBS account, can't test creation"
    
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
