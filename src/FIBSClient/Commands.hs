{-|
This module contains data types representing commands sent to FIBS and functions for working with them. Based
on <http://www.fibs.com/fibs_interface.html>.
-}
module FIBSClient.Commands ( FIBSCommand (..)
                           , Flag (..)
                           , formatCommand
                           ) 
where
import Data.Char

data Flag = Ready
     deriving (Eq, Show)

data FIBSCommand = Toggle Flag
                 | Invite String String
     deriving (Eq, Show)


-- | Yields a string representing given command in format understood by FIBS.
formatCommand :: FIBSCommand -- ^ the command to format
              -> String      -- ^ FIBS command string
formatCommand (Toggle flag) = "toggle " ++ formatFlag flag
formatCommand (Invite user len) = "invite " ++ user ++ " " ++ len

formatFlag :: Flag -> String
formatFlag = map toLower . show
