import CLIP
import Data.Maybe
import Data.Time
import System.Locale
import Test.HUnit

test_parseMessage_failedLoginParsedCorrectly = 
  assertEqual "failed login" (FailedLogin, []) $ parseMessage "login:"
test_parseMessage_welcomeParsedCorrectly = 
  assertEqual "welcome message"
              ((Welcome "username" (fromJust $ parseTime defaultTimeLocale "%s" "1041253132") "1.2.3.4"), [])
              (parseMessage "1 username 1041253132 1.2.3.4")

