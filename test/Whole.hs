{-|
Module      : Whole
Description : Whole program tests.
-}
module Whole (wholeTests) where

import Test.Tasty

import Utils

wholeTests :: TestTree
wholeTests = testGroup "Whole Program Tests" $
    mkWholeProgramTests
      [ newTestGroup "Interaction Tests" "interact" [1,2]
      , newTestGroup "Legal Moves" "moves" [1..8]   
      , newTestGroup "Captures" "capture" [1..7]
      , newTestGroup "Loads and Saves" "loadsave" [1..6]
      , newTestGroup "End Games" "endgame" [1..5]
      , newTestGroup "Special Moves" "special" [1..3]
      ]


--  -------------------------------------------------------------------- [ EOF ]
