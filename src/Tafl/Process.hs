{- |

The `Process` module implements the game commands.

-}
module Tafl.Process
  ( processCommand
  , processCommandStr
  , printError
  ) where

import System.Exit

import Tafl.Core
import Tafl.Logic

-- | Process user commands and updates the GameState.
-- Returns a `TaflError`
processCommand :: GameState
               -> Command
               -> IO (Either TaflError GameState)
processCommand st Help = do
  putStrLn help_text
  pure $ Right st
processCommand st Exit = do
  putStrLn "Good Bye!"
  exitWith ExitSuccess

processCommand st Start = do
   let newSt = st {inGame=True}
   putStrLn "Starting Game."
   pure $ Right newSt

processCommand st Stop = do
   let newSt = st {inGame=False}
   putStrLn "Stopping Game."
   pure $ Right newSt

-- The remaining commands are to be added here.

processCommand st _ = pure $ Left (UnknownCommand)


-- | Process a user given command presented as a String, and update
-- the GameState.
processCommandStr :: GameState
                  -> String
                  -> IO (Either TaflError GameState)
processCommandStr st str =
  case commandFromString str of
    Left err   -> pure (Left err)
    Right cmd' -> processCommand st cmd'


-- | Print an Error to STDOUT.
printError :: TaflError -> IO ()
printError (NotYetImplemented) = do
  putStrLn "Not Yet Implemented."
printError (UnknownCommand) = do
  putStrLn "The command was not recognised"
printError (InvalidCommand msg) = do
  putStrLn "You entered an invalid command:"
  putStr "\t"
  putStrLn msg
