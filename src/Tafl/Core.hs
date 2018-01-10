{- |
This module defines several core data structures used by the game.
-}
{-# LANGUAGE OverloadedStrings #-}

module Tafl.Core
  ( GameState(..)
  , TaflError(..)
  , Command(..)
  , Tile(..)
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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Data.Maybe

-- A tile can be occupied by an Object, a Lambda, a Guard, or could simply be empty
data Tile = O | L | G | E deriving (Show, Eq)

-- | The core game state that captures the state of the board, and
-- whether we are playing a game or not.
--
-- You will need to extend this to present the board.
data GameState = GameState
  { inGame     :: Bool
  , inTestMode :: Bool
  , nextPlayer :: Tile
  , currState  :: [[Tile]]
  }

startState :: [[Tile]]
-- emptyState = replicate 9 (replicate 9 Empty)
startState = [[E, E, E, O, O, O, E, E, E],
              [E, E, E, E, O, E, E, E, E],
              [E, E, E, E, G, E, E, E, E],
              [O, E, E, E, G, E, E, E, O],
              [O, O, G, G, L, G, G, O, O],
              [O, E, E, E, G, E, E, E, O],
              [E, E, E, E, G, E, E, E, E],
              [E, E, E, E, O, E, E, E, E],
              [E, E, E, O, O, O, E, E, E]]

defaultGameState :: Bool -> GameState
defaultGameState b = GameState False b L startState

charToTile :: String -> Tile
charToTile "O" = O
charToTile "L" = L
charToTile "G" = G
charToTile " " = E

-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool -- In test mode or not?
              -> IO (Either TaflError GameState)
initGameState Nothing b = pure $ Right $ defaultGameState b
initGameState (Just f) b = do
  withFile f ReadMode (\handle -> do
    contents <- hGetContents handle
    let fileLines = lines contents
    let turn = head (head fileLines) -- char to represent next player
    let player = (if turn == 'O' then O else G) -- error checking
    let boardList = tail fileLines -- list with csv data
    let boardChar = map (splitOn ",") boardList --[[]]
    let board = map (map charToTile) boardChar
    pure $ Right $ GameState True b player board
    )
  pure $ Left $ UnknownCommand

  -- let convertToList = \(t1, t2, t3, t4, t5, t6, t7, t8, t9) -> [t1, t2, t3, t4, t5, t6, t7, t8, t9]
  -- let boardV = V.map convertToList v --V.forM_ v $ \(t1, t2, t3, t4, t5, t6, t7, t8, t9) -> [t1, t2, t3, t4, t5, t6, t7, t8, t9]
  -- let boardL = V.toList boardV
  -- turn v into a [[Tile]]
  -- construt the game state

    -- V.forM_ v $ \(x,y) -> (putStrLn $ x ++ "'s favourite colour is " ++ y)

tileToChar :: Tile -> String
tileToChar O = "O"
tileToChar L = "L"
tileToChar G = "G"
tileToChar E = " "

returnBoard :: [[Tile]] -> [[String]]
returnBoard board = map (map tileToChar) board

-- | Errors encountered by the game, you will need to extend this to capture *ALL* possible errors.
data TaflError = InvalidCommand String
               | UnknownCommand
               | FileDoesNotExist String
               | NotYetImplemented
               | ForbiddenCommand
               | InvalidMove

-- | REPL commands, you will need to extend this to capture all permissible REPL commands.
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
