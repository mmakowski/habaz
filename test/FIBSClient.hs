import Data.Char
import Data.Maybe
import Data.Time
import FIBSClient
import System.Locale
import Test.HUnit
import Test.QuickCheck
 
testAccount = "habaztest_a"
testPassword = "habaztest"

-- Note: this test will normally be disabled because there is no way to remove 
-- a test account from FIBS once it was created. If other tests fail because
-- the test account does not exist then enable this test to re-create the account
atest_createAccount = 
  do conn <- connect defaultFibsHost defaultFibsPort
     loginStatus <- login conn "habaz-test" testAccount testPassword
     result <- createTestAccount loginStatus conn
     disconnect conn
     return result
  where
    createTestAccount (LoginFailure "invalid login details") _ = assertBool "TODO" False
    createTestAccount (LoginFailure reason) _ = assertFailure reason
    createTestAccount LoginSuccess conn = 
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

test_failedLoginParsedCorrectly = 
  assertEqual "failed login" 
              (ParseSuccess FailedLogin, []) 
              (parseFIBSMessage "login:")

test_blankLineIsSkipped = 
  assertEqual "blank line is skipped"
              (ParseSuccess (System "system message"), [])
              (parseFIBSMessage "\r\n** system message")

test_freeFormParsedCorrectly =
  assertEqual "free form"
              (ParseSuccess (FreeForm "aleks and Ubaretzu start a 1 point match."), [])
              (parseFIBSMessage "\r\naleks and Ubaretzu start a 1 point match.\r\n")
              
test_welcomeParsedCorrectly = 
  assertEqual "CLIP Welcome"
              (ParseSuccess (Welcome "username" (toUTCTime "1041253132") "1.2.3.4"), [])
              (parseFIBSMessage "1 username 1041253132 1.2.3.4\r\n")

test_ownInfoParsedCorrectly = 
  assertEqual "CLIP Own Info"
              (ParseSuccess (OwnInfo "myself" True True False False False False True True 2396 False True False True 3457.85 False False (LimitedTo 0) False False "Australia/Melbourne"), [])
              (parseFIBSMessage "2 myself 1 1 0 0 0 0 1 1 2396 0 1 0 1 3457.85 0 0 0 0 0 Australia/Melbourne\r\n")

test_motdParsedCorrectly = 
  assertEqual "CLIP MOTD"
              (ParseSuccess (MOTD motd), [])
              (parseFIBSMessage $ "3\r\n" ++ motd ++ "4\r\n\r\n")

test_whoInfoParsedCorrectly = 
  assertEqual "CLIP Who Info"
              (ParseSuccess (WhoInfo "mgnu_advanced" (Just "someplayer") Nothing True False 1912.15 827 8 (toUTCTime "1040515752") "192.168.143.5" (Just "3DFiBs") Nothing), [])
              (parseFIBSMessage "5 mgnu_advanced someplayer - 1 0 1912.15 827 8 1040515752 192.168.143.5 3DFiBs -")

test_endOfWhoInfoBlockIsSkipped =
  assertEqual "end of Who Info block is skipped"
              (ParseSuccess (FreeForm "some message"), [])
              (parseFIBSMessage "6\r\n\r\nsome message\r\n")

test_loginParsedCorrectly =
  assertEqual "CLIP Login"
              (ParseSuccess (Login "someplayer" "someplayer logs in."), [])
              (parseFIBSMessage "7 someplayer someplayer logs in.\r\n")

test_logoutParsedCorrectly = 
  assertEqual "CLIP Logout"
              (ParseSuccess (Logout "someplayer" "someplayer drops connection."), [])
              (parseFIBSMessage "8 someplayer someplayer drops connection.\r\n")

test_messageParsedCorrectly = 
  assertEqual "CLIP Message"
              (ParseSuccess (Message "someplayer" (toUTCTime "1041253132") "I'll log in at 10pm if you want to finish that game."), [])
              (parseFIBSMessage "9 someplayer 1041253132 I'll log in at 10pm if you want to finish that game.\r\n")

test_messageDeliveredParsedCorrectly = 
  assertEqual "CLIP Message Delivered"
              (ParseSuccess (MessageDelivered "someplayer"), [])
              (parseFIBSMessage "10 someplayer\r\n")
              
test_messageSavedParsedCorrectly = 
  assertEqual "CLIP Message Saved"
              (ParseSuccess (MessageSaved "someplayer"), [])
              (parseFIBSMessage "11 someplayer\r\n")
              
test_saysParsedCorrectly = 
  assertEqual "CLIP Says"
              (ParseSuccess (Says "someplayer" "Do you want to play a game?"), [])
              (parseFIBSMessage "12 someplayer Do you want to play a game?\r\n")

test_shoutsParsedCorrectly = 
  assertEqual "CLIP Shouts"
              (ParseSuccess (Shouts "someplayer" "Anybody for a 5 point match?"), [])
              (parseFIBSMessage "13 someplayer Anybody for a 5 point match?\r\n")

test_whispersParsedCorrectly = 
  assertEqual "CLIP Whispers"
              (ParseSuccess (Whispers "someplayer" "I think he is using loaded dice  :-)"), [])
              (parseFIBSMessage "14 someplayer I think he is using loaded dice  :-)\r\n")

test_kibitzesParsedCorrectly = 
  assertEqual "CLIP Kibitzes"
              (ParseSuccess (Kibitzes "someplayer" "G'Day and good luck from Hobart, Australia."), [])
              (parseFIBSMessage "15 someplayer G'Day and good luck from Hobart, Australia.\r\n")

test_systemParsedCorrectly =
  assertEqual "System **"
              (ParseSuccess (System "You're now ready to invite or join someone."), [])
              (parseFIBSMessage "** You're now ready to invite or join someone.\r\n")

test_parseFIBSMessagesIsLazy = 
  assertEqual "parseFIBSMessages is lazy" 
              [ParseSuccess FailedLogin, ParseSuccess FailedLogin]
              (take 2 $ parseFIBSMessages $ cycle "login:")

instance Test.QuickCheck.Arbitrary Command where
  arbitrary = elements [Toggle Ready] -- TODO: more examples
  coarbitrary _ = id -- TODO

prop_formatCommand cmd@(Toggle flag) = "toggle " ++ (map toLower $ show flag) == formatCommand cmd

toUTCTime str = (fromJust $ parseTime defaultTimeLocale "%s" str)

motd =
  "+--------------------------------------------------------------------+\r\n\
  \|                                                                    |\r\n\
  \| It was a dark and stormy night in Oakland.  Outside, the rain      |\r\n\
  \| came down in torrents.  Winds of 40MPH+ pounded at the windows,    |\r\n\
  \| and whipped at trees, power lines, and anyone foolish enough       |\r\n\
  \| to be outdoors.                                                    |\r\n\
  \|                                                                    |\r\n\
  \| In the middle of the night, I was awakened by a loud BEEP BEEP     |\r\n\
  \| BEEP BEEP.  \"What is that?\" I thought groggily.  Ah!  We've had    |\r\n\
  \| a power failure, and the computers are running on battery power.   |\r\n\
  \|                                                                    |\r\n\
  \| Curious to see how things were working, I crawled out of bed and   |\r\n\
  \| stumbled downstairs to log in.  To my delight, people were         |\r\n\
  \| merrily playing backgammon, oblivious to the fact that they had    |\r\n\
  \| just ridden out a power failure that a few months ago would have   |\r\n\
  \| shut down the server most ungracefully.                            |\r\n\
  \|                                                                    |\r\n\
  \| Thanks to all of the generous FIBSters who bought the UPS and      |\r\n\
  \| made this possible!  And coming soon, as soon as I can get it      |\r\n\
  \| built and deployed, a new (more reliable, and maybe faster)        |\r\n\
  \| server.                                                            |\r\n\
  \|                                                                    |\r\n\
  \+--------------------------------------------------------------------+\r\n"


