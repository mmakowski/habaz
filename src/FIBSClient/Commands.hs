{-|
This module contains data types representing commands sent to FIBS and functions for working with them. Based
on <http://www.fibs.com/fibs_interface.html>.
-}
module FIBSClient.Commands (
  FIBSCommand(..),  
  Flag(..),
  formatCommand
) where
import Data.Char
import System.Random
import Test.QuickCheck

data Flag
     = Ready
     deriving (Eq, Show)

instance Test.QuickCheck.Arbitrary Flag where
  arbitrary = elements [Ready]
  coarbitrary _ = id -- not needed

data FIBSCommand
     = Toggle Flag
     deriving (Eq, Show)

instance Test.QuickCheck.Arbitrary FIBSCommand where
  arbitrary = elements toggles
    where 
      toggles = map (\f -> Toggle f) $ generate 20 (System.Random.mkStdGen 0) arbitrary
  coarbitrary _ = id -- not needed


-- | Yields a string representing given command in format understood by FIBS.
formatCommand :: FIBSCommand -- ^ the command to format
              -> String      -- ^ FIBS command string
formatCommand (Toggle flag) = "toggle " ++ (formatFlag flag)

prop_formatCommandToggleFlag cmd@(Toggle flag) = "toggle " ++ (map toLower $ show flag) == formatCommand cmd

formatFlag :: Flag -> String
formatFlag = map toLower . show
