module Main where
import Test.Framework (Test, defaultMain)
-- test modules
import FIBSClientTests
import ModelTests

main :: IO () 
main = defaultMain tests

tests :: [Test]
tests = allFIBSClientTests ++ allModelTests
