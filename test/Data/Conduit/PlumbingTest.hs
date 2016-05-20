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

testFeedback :: a -> Conduit (b, a) Identity (c, a) -> [b] -> Identity [c]
testFeedback i c xs = yieldMany xs =$= feedback i c $$ sinkList

t_feedback :: Test.Framework.Test
t_feedback = testGroup "feedback"
  [ testCase "feedback" $ (pure [1..5] @?=
                           testFeedback 0 (CC.map ((\x -> (x, x)) . (+1) . snd)) [1, 1, 1, 1, 1])
  ]
