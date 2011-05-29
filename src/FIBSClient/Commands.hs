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
import System.Random
import Test.QuickCheck

data Flag
     = Ready
     deriving (Eq, Show)

instance Arbitrary Flag where
  arbitrary = elements [Ready]

data FIBSCommand
     = Toggle Flag
     deriving (Eq, Show)

instance Arbitrary FIBSCommand where
  arbitrary = toggles
    where 
      toggles = 
        do f <- arbitrary 
           return (Toggle f)


-- | Yields a string representing given command in format understood by FIBS.
formatCommand :: FIBSCommand -- ^ the command to format
              -> String      -- ^ FIBS command string
formatCommand (Toggle flag) = "toggle " ++ (formatFlag flag)

prop_formatCommandToggleFlag cmd@(Toggle flag) = "toggle " ++ (map toLower $ show flag) == formatCommand cmd

formatFlag :: Flag -> String
formatFlag = map toLower . show
