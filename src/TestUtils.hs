module TestUtils where
import Data.Maybe
import Data.Time
import System.Locale

toUTCTime str = (fromJust $ parseTime defaultTimeLocale "%s" str)

