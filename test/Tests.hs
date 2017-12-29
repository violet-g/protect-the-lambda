{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Stats
import Test.Tasty.Runners.AntXML
import Test.Tasty.Runners.Html

import Internal
import Whole

--  ------------------------------------------------------------------- [ Main ]

ingredients :: [Ingredient]
ingredients =  [composeReporters htmlRunner $ composeReporters antXMLRunner consoleStatsReporter]
           ++ [rerunningTests [consoleTestReporter]]
           ++ defaultIngredients

main :: IO ()
main = defaultMainWithIngredients ingredients $
  testGroup "Protect the Lamba Test Suite"
            [internalTests, wholeTests]


  --  -------------------------------------------------------------------- [ EOF ]
