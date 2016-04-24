module Data.Conduit.Arrow.CombinatorsTest where

import           Control.Arrow
import           Data.Conduit
import           Data.Conduit.Combinators (yieldMany, sinkList)
import           Data.Conduit.Arrow
import qualified Data.Conduit.Arrow.Combinators as AC
import           Control.Monad.Identity

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

suite :: Test.Framework.Test
suite = testGroup "Data.Conduit.Arrow.Combinators"
  [ t_map
  ]

testArrow :: ConduitArrow Identity Int b -> [Int] -> Identity [b]
testArrow a xs = yieldMany xs =$>> a $$ sinkList

t_map :: Test.Framework.Test
t_map = testGroup "map"
  [ testCase "id" $ (pure [1..5] @?= testArrow (AC.map id) [1..5])
  ]
