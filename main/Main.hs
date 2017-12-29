module Main where

import System.Exit

import Tafl.Core (initGameState)
import Tafl.Process
import Tafl.Interact
import Tafl.Options

main :: IO ()
main = do
  (GameOptions inTestMode mState) <- parseGameOptions
  result <- initGameState mState inTestMode
  case result of
    Left err     -> printError err
    Right initSt -> do
      repl initSt
      exitWith ExitSuccess
