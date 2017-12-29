{-|
Module      : Internal
Description : Internal Tests
License     : See LICENSE
-}
module Internal (internalTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Tafl.Logic


internalTests :: TestTree
internalTests =
  testGroup
    "Testing Internals"
    [exampleTests1, exampleTests2]


exampleTests1 :: TestTree
exampleTests1 =
  testGroup
    "Testing Command Execution 1"
    [
      testCase "Failing Test" (assertEqual "" False True)
    , testCase "Passing Test" (assertEqual "" True True)
    ]

exampleTests2 :: TestTree
exampleTests2 =
  testGroup
    "Testing Command Execution 2"
    [
      testCase "Failing Test" (assertEqual ""False True)
    , testCase "Passing Test" (assertEqual ""True True)
    ]


-- -------------------------------------------------------------------- [ EOG ]
