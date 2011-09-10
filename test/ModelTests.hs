module ModelTests where
import Control.Monad (liftM, liftM2, liftM5)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Model

import FIBSClientTests -- arbitrary instance for ParseResults

type DummyConn = ()

instance Arbitrary PlayerName where
  arbitrary = liftM PlayerName arbitrary

instance Arbitrary PlayerGameState where 
  arbitrary = do player <- arbitrary
                 elements [ None, Playing player, Watching player ]

instance Arbitrary PlayerDelta where
  arbitrary = do pName <- arbitrary
                 elements [ Added pName, Removed pName, Updated pName ]

instance Arbitrary PlayerInfo where
  arbitrary = liftM5 PlayerInfo arbitrary arbitrary arbitrary arbitrary arbitrary
                   
instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Map a b) where
  arbitrary = oneof [ return Map.empty, liftM Map.fromList $ listOf arbitrary ]
  
instance Arbitrary Players where
  arbitrary = liftM2 Players arbitrary arbitrary  

instance Arbitrary c => Arbitrary (SessionStateF c) where
  arbitrary = do errors <- arbitrary
                 conn <- arbitrary
                 msgs <- arbitrary
                 players <- arbitrary
                 elements [ Disconnected errors 
                          , LoggedOut conn errors
                          , LoggedIn conn msgs errors
                          , ProcessingMessages conn players errors
                          , NotReady conn players errors
                          , Ready conn players errors
                          ]

allModelTests :: [Test]
allModelTests = [ stateManipulation ]

stateManipulation = testGroup "Session state manipulation" [
  testProperty "popError pops what logError logged" ppwll 
  ]
  where
    ppwll :: SessionStateF DummyConn -> Bool
    ppwll st = popError (logError "latest error" st) == (Just "latest error", st)
