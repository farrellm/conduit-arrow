import Data.Conduit.Arrow.CombinatorsTest
import Data.Conduit.PlumbingTest

import Test.HUnit
import Test.Framework
-- import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain
  [ Data.Conduit.Arrow.CombinatorsTest.suite
  , Data.Conduit.PlumbingTest.suite
  ]
