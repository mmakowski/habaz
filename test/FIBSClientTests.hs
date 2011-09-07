module FIBSClientTests where
import Control.Monad (liftM, liftM2, liftM3)
import Data.Char
import Data.Maybe
import Data.Time
import System.Locale
import System.Random
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import FIBSClient.Commands
import FIBSClient.Messages

instance Arbitrary RedoubleLimit where
  arbitrary = oneof [ return Unlimited, liftM LimitedTo $ choose (0, 20) ]

instance Arbitrary FIBSMessage where
  arbitrary = oneof [ return LoginPrompt 
                    , liftM FreeForm arbitrary 
                    , liftM System arbitrary
                    , return ReadyOn
                    , return ReadyOff
                    , return ConnectionTimeOut
                    , return EndOfGoodbyeMessage
                    -- TODO: , liftM3 Welcome arbitrary arbitrary arbitrary
                    -- TODO: OwnInfo
                    , liftM MOTD arbitrary
                    -- TODO: WhoInfo
                    , return EndOfWhoInfoBlock
                    , liftM2 Login arbitrary arbitrary
                    , liftM2 Logout arbitrary arbitrary
                    -- TODO: , liftM3 Message arbitrary arbitrary arbitrary
                    , liftM MessageDelivered arbitrary
                    , liftM MessageSaved arbitrary
                    , liftM2 Says arbitrary arbitrary
                    , liftM2 Shouts arbitrary arbitrary
                    , liftM2 Whispers arbitrary arbitrary
                    , liftM2 Kibitzes arbitrary arbitrary
                    , liftM2 YouSay arbitrary arbitrary
                    , liftM YouShout arbitrary
                    , liftM YouWhisper arbitrary
                    , liftM YouKibitz arbitrary
                    ]

-- TODO: separate instance for [FibsMessage], with realistic message sequences 

-- TODO: define where it's needed
instance Arbitrary a => Arbitrary (ParseResult a) where
  arbitrary = do a <- arbitrary
                 msg <- arbitrary
                 elements [ParseSuccess a, ParseFailure msg]


allFIBSClientTests :: [Test]
allFIBSClientTests = [ parsingTopLevel
                     , individualMessageTypesParsing
                     , commandFormatting
                     ]
        
parsingTopLevel = testGroup "FIBS message stream parsing and splitting" [
  testCase "parseFIBSMessages is lazy" (
     [ParseSuccess LoginPrompt, ParseSuccess LoginPrompt] @=? (take 2 . parseFIBSMessages . cycle) "login:"),

  testCase "splitByFirst preserves failures" (
    ([ParseFailure "0", ParseSuccess 1], 2, [ParseFailure "3"]) @=?
    ([ParseFailure "0", ParseSuccess 1, ParseSuccess 2, ParseFailure "3"] `splitByFirst` (== 2))),

  testCase "splitByFirst with limit stops at limit" (
    Nothing @=? splitByFirstWithLimit [ParseSuccess 1, ParseSuccess 2, ParseSuccess 3, ParseSuccess 4] (== 3) 2),

  testCase "splitByFirst with limit finds element at limit" (
    Just ([ParseSuccess 1, ParseSuccess 2], 3, [ParseSuccess 4]) @=?
    splitByFirstWithLimit [ParseSuccess 1, ParseSuccess 2, ParseSuccess 3, ParseSuccess 4] (== 3) 3)
  ]


individualMessageTypesParsing = testGroup "FIBS message parsing of individual message types" [
  testCase "login prompt" ("login:" `parsesTo` LoginPrompt),
  
  testCase "system message" ("** Some system message.\r\n" `parsesTo` System "Some system message."),
  
  testCase "ready on" ("** You're now ready to invite or join someone.\r\n" `parsesTo` ReadyOn),
  
  testCase "ready off" ("** You're now refusing to play with someone.\r\n" `parsesTo` ReadyOff),
  
  testCase "blank line before system message is skipped" 
    ("\r\n** system message" `parsesTo` System "system message"),

  testCase "connection time out" ("Connection timed out." `parsesTo` ConnectionTimeOut),
  
  testCase "end of goodbye message" 
    ("                      Keep them coming....\r\n" `parsesTo` EndOfGoodbyeMessage),

  testCase "free form"
    ("\r\naleks and Ubaretzu start a 1 point match.\r\n" `parsesTo` 
     FreeForm "aleks and Ubaretzu start a 1 point match."),
              
  testCase "welcome"
    ("1 username 1041253132 1.2.3.4\r\n" `parsesTo` Welcome "username" (toUTCTime "1041253132") "1.2.3.4"),
     
  testCase "own info"
    ("2 myself 1 1 0 0 0 0 1 1 2396 0 1 0 1 3457.85 0 0 0 0 0 Australia/Melbourne\r\n" `parsesTo`
     OwnInfo "myself" True True False False False False True True 2396 False True False True 3457.85 False False (LimitedTo 0) False False "Australia/Melbourne"),
  
  testCase "motd" (("3\r\n" ++ motd ++ "4\r\n\r\n") `parsesTo` MOTD motd),
  
  testCase "who info"
    ("5 mgnu_advanced someplayer - 1 0 1912.15 827 8 1040515752 192.168.143.5 3DFiBs -" `parsesTo`
     WhoInfo "mgnu_advanced" (Just "someplayer") Nothing True False 1912.15 827 8 (toUTCTime "1040515752") "192.168.143.5" (Just "3DFiBs") Nothing),

  testCase "who info parse failure"
    (("5 " ++ badWhoInfoLine) `failsToParseWithErr` 
     ("unable to parse " ++ (show . words) badWhoInfoLine ++ " as WhoInfo")),

  testCase "end of who info block is skipped" ("6\r\n\r\nsome message\r\n" `parsesTo` FreeForm "some message"),

  testCase "login" ("7 someplayer someplayer logs in.\r\n" `parsesTo` Login "someplayer" "someplayer logs in."),

  testCase "logout" 
    ("8 someplayer someplayer drops connection.\r\n" `parsesTo` 
     Logout "someplayer" "someplayer drops connection."),

  testCase "message" 
    ("9 someplayer 1041253132 I'll log in at 10pm if you want to finish that game.\r\n" `parsesTo` 
     Message "someplayer" (toUTCTime "1041253132") "I'll log in at 10pm if you want to finish that game."),

  testCase "message delivered" ("10 someplayer\r\n" `parsesTo` MessageDelivered "someplayer"),
              
  testCase "message saved" ("11 someplayer\r\n" `parsesTo` MessageSaved "someplayer"),
              
  testCase "says" 
    ("12 someplayer Do you want to play a game?\r\n" `parsesTo` Says "someplayer" "Do you want to play a game?"),

  testCase "shouts"
    ("13 someplayer Anybody for a 5 point match?\r\n" `parsesTo` 
     Shouts "someplayer" "Anybody for a 5 point match?"),

  testCase "whispers"
    ("14 someplayer I think he is using loaded dice  :-)\r\n" `parsesTo` 
     Whispers "someplayer" "I think he is using loaded dice  :-)"),

  testCase "kibitzes"
    ("15 someplayer G'Day and good luck from Hobart, Australia.\r\n" `parsesTo` 
     Kibitzes "someplayer" "G'Day and good luck from Hobart, Australia.")
  ]
  where 
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
    
    badWhoInfoLine = "mgnu_advanced someplayer - 1 0 1912.15 lubudubu logs in."
    
    str `parsesTo` msg = assertEqual "" (ParseSuccess msg, []) (parseFIBSMessage str)
    
    str `failsToParseWithErr` err = assertEqual "" (ParseFailure err, []) (parseFIBSMessage str)

    toUTCTime str = fromJust $ parseTime defaultTimeLocale "%s" str


instance Arbitrary Flag where
  arbitrary = elements [Ready]

instance Arbitrary FIBSCommand where
  arbitrary = toggles
    where 
      toggles = 
        do f <- arbitrary 
           return (Toggle f)

commandFormatting = testGroup "FIBS command formatting" [
  testProperty "formatted toggle is 'toggle <flag>'" formatCommandToggleFlag
  ]
  where
    formatCommandToggleFlag cmd@(Toggle flag) = "toggle " ++ (map toLower . show) flag == formatCommand cmd


-- tests

testAccount = "habaztest_a"
testPassword = "habaztest"

-- Note: this test will normally be disabled because there is no way to remove 
-- a test account from FIBS once it was created. If other tests fail because
-- the test account does not exist then enable this test to re-create the account
{-
atest_createAccount = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
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
-}    
{-
test_accountCreation = 
  do conn <- connect defaultFIBSHost defaultFIBSPort
     --readUntil "login:" conn
     send conn "guest\n"
     str <- readUntil "!!!" conn
     -- putStr str
     send conn "bye\n"
     disconnect conn
     return $ assertBool "not logged in as guest" $ isInfixOf "You just logged in as guest" str

-}

