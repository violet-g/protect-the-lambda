{-|
Module      : Utils
Description : Utilities for performing whole file testing.
Copyright   : (c) University of Glasgow, 2017
License     : See LICENSE
Maintainer  : Jan de Muijnck-Hughes
-}
module Utils
  (
    mkWholeProgramTests
  , newTestGroup
  ) where

import Control.Monad

import Data.Char (isLetter)
import Data.List
import Data.Monoid ((<>))
import Data.Proxy
import Data.Typeable

import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.Info
import System.IO
import System.Process

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Options
import Test.Tasty.Runners

testDirectory :: String
testDirectory = "test"

-- | Create a new test group description.
newTestGroup :: String -- ^ Pretty Name of test groupfolder
             -> String -- ^ name of directory containing test
             -> [Int]  -- ^ Number of Tests
             -> (String, String, [Int])
newTestGroup pName rName is = (pName, rName, is)

--  ------------------------------------------------------------------- [ Core ]

-- | A wrapper around @goldenVsFile@ from @Tasty.Golden@ to make it
-- cross platform.
test :: String    -- ^ Name of the test to run.
     -> String    -- ^ Location of the test directory.
     -> IO ()     -- ^ The test to run, that creates the output.
     -> TestTree
test testName path = goldenVsFileDiff testName diff ref output
  where
    ref    = path </> "expected"
    output = path </> "output"

    diff ref new | os == "openbsd" = ["diff", "-u", new, ref]
                 | os == "darwin" = ["diff", "-u", "-w", new, ref]
                 | otherwise = ["diff", "-w", "--strip-trailing-cr", "-u", new, ref]

-- | Helper function to ensure test numbers are three digits in length
-- and have leading zero's when applicable.
indexToString :: Int -> String
indexToString index =
  let str = show index
  in  replicate (3 - length str) '0' ++ str

-- | Turns the collection of TestFamily into actual tests usable by
-- Tasty.
mkWholeProgramTests :: [(String, String, [Int])]
                    -> [TestTree]
mkWholeProgramTests = fmap mkTestFamily
    where
      mkTestFamily :: (String, String, [Int]) -> TestTree
      mkTestFamily (pname, dname, tests) =
        testGroup pname (fmap (mkTest dname) tests)

      mkTest :: String -> Int -> TestTree
      mkTest dname index =
        let testname = dname ++ "-" ++ indexToString index
            path = testDirectory </> testname
         in
          test testname path (runTest path)


-- | Runs a test script
--
-- "bash" needed because Haskell has cmd as the default shell on
-- windows, and we also want to run the process with another current
-- directory, so we get this thing.
runTest :: String -> IO ()
runTest path = do
     let run = (proc "bash" ["run"]) {cwd = Just path}

     (_, output, error_out) <- readCreateProcessWithExitCode run ""

     writeFile (path </> "output") (normalise output)
     when (error_out /= "") $
         hPutStrLn stderr ("\nError: " ++ path ++ "\n" ++ error_out)
   where
      -- Normalise paths
      normalise ('.' : '\\' : c : xs) | isLetter c  = '.' : '/' : c : normalise xs
      normalise ('\\':'\\':xs) = '/' : normalise xs
      normalise (x : xs) = x : normalise xs
      normalise [] = []

--  -------------------------------------------------------------------- [ EOF ]
