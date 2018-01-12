{- |
This module defines several core data structures used by the game.
-}

module Tafl.Core
  ( GameState(..)
  , TaflError(..)
  , Command(..)
  , Tile(..)
  , Player(..)
  , defaultGameState
  , initGameState
  , commandFromString
  , help_text
  , returnBoard
  ) where

import qualified Data.ByteString.Lazy.Char8 as BLC (lines, unpack)
import System.IO
import System.Directory
import System.Exit
import Data.List
import Data.List.Split (splitOn)
import Data.Csv
import Data.Maybe

-- | A tile can be occupied by an Object, a Lambda, a Guard, or could simply be empty
data Tile = O | L | G | E deriving (Show, Eq)

-- | A player is either an attacker, or a Defender
data Player = Attacker | Defender deriving (Show,Eq)

-- | The core game state that captures the state of the board, and
-- | whether we are playing a game or not.
data GameState = GameState
  { inGame     :: Bool
  , inTestMode :: Bool
  , player :: Player
  , currState  :: [[Tile]]
  }

-- | Default board
startState :: [[Tile]]
startState = [[E, E, E, O, O, O, E, E, E],
              [E, E, E, E, O, E, E, E, E],
              [E, E, E, E, G, E, E, E, E],
              [O, E, E, E, G, E, E, E, O],
              [O, O, G, G, L, G, G, O, O],
              [O, E, E, E, G, E, E, E, O],
              [E, E, E, E, G, E, E, E, E],
              [E, E, E, E, O, E, E, E, E],
              [E, E, E, O, O, O, E, E, E]]

-- | Game state at the beginning
defaultGameState :: Bool -> GameState
defaultGameState b = GameState True b Attacker startState

initGameState :: Maybe FilePath
              -> Bool -- In test mode or not?
              -> IO (Either TaflError GameState)
initGameState Nothing b = pure $ Right $ defaultGameState b
initGameState (Just f) b = do
  withFile f ReadMode (\handle -> do
    contents <- hGetContents handle
    let fileLines = lines contents
    let turn = head (head fileLines)
    let player = (if turn == 'O' then Attacker else Defender)
    let board = map (map charToTile) (map (splitOn ",") (tail fileLines))
    pure $ Right $ GameState True b player board
    )
  --pure $ Left $ CommandCannotBeUsed


{-
-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool -- In test mode or not?
              -> IO (Either TaflError GameState)
initGameState Nothing b = pure $ Right $ defaultGameState b
initGameState (Just f) b = do
  result <- try (res) :: IO (Either Exception Int)
    where res = withFile f ReadMode (\handle -> do
            contents <- hGetContents handle
            let turn = head (head (lines contents)) -- char to represent next player
            let player = (if turn == 'O' then Attacker else Defender) -- error checking
            let boardList = tail fileLines -- list with csv data
            let boardChar = map (splitOn ",") boardList --[[]]
            let board = map (map charToTile) boardChar
      --putStrLn "Inside first"
    )
    case result of
      Left ex  -> pure $ Right $ GameState True b player board
      Right val -> pure $ Left $ CannotLoadSavedGame
-}

charToTile :: String -> Tile
charToTile "O" = O
charToTile "L" = L
charToTile "G" = G
charToTile _ = E

tileToChar :: Tile -> String
tileToChar O = "O"
tileToChar L = "L"
tileToChar G = "G"
tileToChar E = " "

-- | Return the board ready to be printed
returnBoard :: [[Tile]] -> [[String]]
returnBoard board = map (map tileToChar) board

-- | Errors encountered by the game
data TaflError = InvalidCommand String
               | UnknownCommand
               | FileDoesNotExist String
               | NotYetImplemented
               | ForbiddenCommand
               | InvalidMove
               | CommandCannotBeUsed
               | CannotLoadSavedGame

-- | REPL commands
data Command = Help
             | Exit
             | Start
             | Stop
             | Move String String
             | Save String
             | Load String

-- | Try to construct a command from the given string.
commandFromString :: String -> Either TaflError Command
commandFromString (':':rest) =
  case words rest of
    ["help"] -> Right Help
    ["exit"] -> Right Exit
    ["start"] -> Right Start
    ["stop"]  -> Right Stop
    ["move", src, dest] -> Right $ Move src dest
    ["save", fname] -> Right $ Save fname
    ["load", fname] -> Right $ Load fname
    _ -> Left UnknownCommand

commandFromString _  = Left UnknownCommand

help_text :: String
help_text = unlines $
     [ "Tafl Help text:", ""]
  ++ map prettyCmdHelp
       [ ("help",  "Displays this help text." )
       , ("exit",  "Exits the Command Prompt.")
       , ("start", "Initiates a game."        )
       , ("stop",  "Stops a game."            )
       , ("move",  "Moves a piece on the board.")
       , ("save",  "Saves the state in a csv file.")
       , ("load",  "Loads the state from a csv file.")
       ]
  where
    prettyCmdHelp :: (String, String) -> String
    prettyCmdHelp (cmd, help) = concat ["\t:", cmd, "\t", " "] ++ help

