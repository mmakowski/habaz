import CLIP

import Data.Maybe
import Data.Time
import System.Locale
import Test.HUnit

test_parseMessage_failedLoginParsedCorrectly = 
  assertEqual "failed login" 
              (Success FailedLogin, []) 
              (parseMessage "login:")

test_parseMessage_welcomeParsedCorrectly = 
  assertEqual "CLIP Welcome"
              (Success (Welcome "username" (fromJust $ parseTime defaultTimeLocale "%s" "1041253132") "1.2.3.4"), [])
              (parseMessage "1 username 1041253132 1.2.3.4")

test_parseMessage_ownInfoParsedCorrectly = 
  assertEqual "CLIP Own Info"
              (Success (OwnInfo "myself" True True False False False False True True 2396 False True False True 3457.85 False False (LimitedTo 0) False False "Australia/Melbourne"), [])
              (parseMessage "2 myself 1 1 0 0 0 0 1 1 2396 0 1 0 1 3457.85 0 0 0 0 0 Australia/Melbourne")

test_parseMessages_isLazy = 
  assertEqual "parseMessages is lazy" 
              [Success FailedLogin, Success FailedLogin]
              (take 2 $ parseMessages $ cycle "login:")

