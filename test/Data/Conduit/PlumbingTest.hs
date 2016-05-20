module Data.Conduit.PlumbingTest where

import           Data.Conduit
import           Data.Conduit.Combinators (yieldMany, sinkList)
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit.Plumbing
import           Control.Monad.Identity

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

suite :: Test.Framework.Test
suite = testGroup "Data.Conduit.Plumbing"
  [ t_feedback
  ]

testFeedback :: Conduit (a, c) Identity (b, c) -> c -> [a] -> Identity [b]
testFeedback c i xs = yieldMany xs =$= feedback c i $$ sinkList

t_feedback :: Test.Framework.Test
t_feedback = testGroup "feedback"
  [ testCase "feedback" $ (pure [1..5] @?=
                           testFeedback (CC.map ((\x -> (x, x)) . (+1) . snd)) 0 [1, 1, 1, 1, 1])
  ]
