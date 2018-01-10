{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (

  ) where

import Tafl.Core

type Position = (Int, Int)

move :: GameState -> String -> String -> Either TaflError GameState
move gs src = isCorner (strToCoordinates src)
-- convert

strToCoordinates :: String -> Position
strToCoordinates str = ((ord (head str) - 96), (ord (head (tail str)) - 48))

isCorner :: Position -> Bool
isCorner pos = pos == (1,1) || pos == (1,9) || pos == (9,1) || pos == (9,9)

isMiddlePiece :: Position -> Bool
isMiddlePiece pos = pos == (5,5)

-- checks
--Invalid move:
-- corner
-- middle piece
-- dest empty
-- outside of board
-- piece in the way -- path is not empty and dest is not empty
