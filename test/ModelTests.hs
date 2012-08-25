module ModelTests where 
import Control.Monad (liftM)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck

import Model
import Events

allModelTests :: [Test]
allModelTests = []
