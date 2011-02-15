import CLIP

import Data.Maybe
import Data.Time
import System.Locale
import Test.HUnit

test_failedLoginParsedCorrectly = 
  assertEqual "failed login" 
              (Success FailedLogin, []) 
              (parseMessage "login:")

test_welcomeParsedCorrectly = 
  assertEqual "CLIP Welcome"
              (Success (Welcome "username" (toUTCTime "1041253132") "1.2.3.4"), [])
              (parseMessage "1 username 1041253132 1.2.3.4")

test_ownInfoParsedCorrectly = 
  assertEqual "CLIP Own Info"
              (Success (OwnInfo "myself" True True False False False False True True 2396 False True False True 3457.85 False False (LimitedTo 0) False False "Australia/Melbourne"), [])
              (parseMessage "2 myself 1 1 0 0 0 0 1 1 2396 0 1 0 1 3457.85 0 0 0 0 0 Australia/Melbourne")

test_motdParsedCorrectly = 
  assertEqual "CLIP MOTD"
              (Success (MOTD motd), [])
              (parseMessage $ "3\r\n" ++ motd ++ "4\r\n")

test_whoInfoParsedCorrectly = 
  assertEqual "CLIP Who Info"
              (Success (WhoInfo [
                           (PlayerInfo "mgnu_advanced" (Just "someplayer") Nothing True False 1912.15 827 8 (toUTCTime "1040515752") "192.168.143.5" (Just "3DFiBs") Nothing),
                           (PlayerInfo "someplayer" (Just "mgnu_advanced") Nothing False False 1418.61 23 1914 (toUTCTime "1041272421") "192.168.40.3" (Just "MacFIBS") (Just "someplayer@somewhere.com")),
                           (PlayerInfo "anotherplayer" Nothing Nothing False False 1439.79 1262 410 (toUTCTime "1041251697") "somehost.com" Nothing Nothing)]), [])
              (parseMessage whoInfoMsg)

test_parseMessagesIsLazy = 
  assertEqual "parseMessages is lazy" 
              [Success FailedLogin, Success FailedLogin]
              (take 2 $ parseMessages $ cycle "login:")


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

whoInfoMsg = 
  "5 mgnu_advanced someplayer - 1 0 1912.15 827 8 1040515752 192.168.143.5 3DFiBs -\r\n\
  \5 someplayer mgnu_advanced - 0 0 1418.61 23 1914 1041272421 192.168.40.3 MacFIBS someplayer@somewhere.com\r\n\
  \5 anotherplayer - - 0 0 1439.79 1262 410 1041251697 somehost.com - -\r\n\
  \6\r\n"