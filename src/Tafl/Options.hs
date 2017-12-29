{- |

This module defines the game's CLI.

-}
module Tafl.Options
  (
    GameOptions(..)
  , parseGameOptions
  ) where

import Data.Monoid ((<>))

import Options.Applicative

cmdParser  :: Parser GameOptions
cmdParser = GameOptions
  <$> switch (long "test" <> help "Run the game in test mode.")
  <*> optional (strOption (long "state"
             <> metavar "FILE"
             <> help "A CSV file containing the initial boars state." ))

versionFlag = infoOption "0.1.0.0"
                   (short 'v'
                <> long "version"
                <> help "Print version information.")

parseGameOptions :: IO GameOptions
parseGameOptions = execParser opts
  where
    opts = info (versionFlag <*> helper <*> cmdParser)
                (fullDesc
                   <> progDesc "A CLI for Playing Protect the Lambda."
                   <> header "protect-the-lambda")

data GameOptions =
  GameOptions {
      inTestMode  :: Bool
    , initState   :: Maybe FilePath
    }
