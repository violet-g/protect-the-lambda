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
import Control.Monad

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

processCommand st (Load f) =
  initGameState (getFilePath f) False

processCommand st (Move src dst) = do
  -- | Check if game is active
  if (inGame st == False) then pure $ Left (CommandCannotBeUsed)
  else do
    let newSt = move st src dst
    -- | See if anyone won, and if not, allow  next moves
    case newSt of
      Left taflError -> putStr ""
      Right gameState -> do
        if (inGame gameState == False && (player gameState) == Attacker) then putStrLn "Lambdas Win!" 
        else if (inGame gameState == False && (player gameState) == Defender) then putStrLn "Objects Win!" 
        else do
          when (not $ inTestMode gameState) $ mapM_ print (returnBoard $ currState gameState)
          putStrLn "Move Successful"
    pure $ newSt

processCommand st _ = pure $ Left (UnknownCommand)

getFilePath :: String -> Maybe FilePath
getFilePath f = Just f :: Maybe FilePath

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
printError (ForbiddenCommand) = do
  putStrLn "The command cannot be used"
printError (InvalidMove) = do
  putStrLn "Invalid move."
printError (CommandCannotBeUsed) = do
  putStrLn "The command cannot be used."
printError (CannotLoadSavedGame) = do
  putStrLn "Cannot load saved game."
