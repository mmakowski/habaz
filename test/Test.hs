module Main where
import Test.Framework (Test, defaultMain)
-- test modules
import FIBSClientTests

main :: IO () 
main = defaultMain tests

tests :: [Test]
tests = [ parsingTopLevel
        , individualMessageTypesParsing
        , commandFormatting
        ]
