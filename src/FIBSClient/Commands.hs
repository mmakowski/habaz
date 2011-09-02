{-|
This module contains data types representing commands sent to FIBS and functions for working with them. Based
on <http://www.fibs.com/fibs_interface.html>.
-}
module FIBSClient.Commands (
  -- * Types
  FIBSCommand(..),  
  Flag(..),
  -- * Functions
  formatCommand
) where
import Data.Char

data Flag
     = Ready
     deriving (Eq, Show)

data FIBSCommand
     = Toggle Flag
     deriving (Eq, Show)


-- | Yields a string representing given command in format understood by FIBS.
formatCommand :: FIBSCommand -- ^ the command to format
              -> String      -- ^ FIBS command string
formatCommand (Toggle flag) = "toggle " ++ formatFlag flag

formatFlag :: Flag -> String
formatFlag = map toLower . show
