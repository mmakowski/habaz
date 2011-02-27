module TestUtils where
import Data.Maybe
import Data.Time
import System.Locale
import Test.QuickCheck

toUTCTime str = (fromJust $ parseTime defaultTimeLocale "%s" str)

instance Arbitrary Char where
  arbitrary = elements [' '..'z']
  coarbitrary _ = id -- not needed  

