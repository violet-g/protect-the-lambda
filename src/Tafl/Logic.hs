{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (
  move
  ) where

import Tafl.Core
import Data.Char

type Position = (Int, Int)

move :: GameState -> String -> String -> Either TaflError GameState
move st src dst
  | isValidMove st (strToCoordinates src) (strToCoordinates dst) = Right $ updateState st (strToCoordinates src) (strToCoordinates dst)
  | otherwise = Left $ InvalidMove

isValidMove :: GameState -> Position -> Position -> Bool
isValidMove st src dst = (not $ isMiddlePiece dst)  && (not $ isCorner dst) && (not $ isOutOfBounds src) && (not $ isOutOfBounds dst) && (not $ src == dst) && ((isMoveHorizontal src dst) || (isMoveVertical src dst)) && (isPathEmpty st src dst)

strToCoordinates :: String -> Position
strToCoordinates str = ((ord (head str) - 96), 10 - (ord (head (tail str)) - 48))

isInGame :: GameState -> Bool
isInGame st = inGame st == False

isCorner :: Position -> Bool
isCorner pos = pos == (1,1) || pos == (1,9) || pos == (9,1) || pos == (9,9)

isMiddlePiece :: Position -> Bool
isMiddlePiece pos = pos == (5,5)

isOutOfBounds :: Position -> Bool
isOutOfBounds pos = fst pos < 1 || fst pos > 9 || snd pos < 1 || snd pos > 9

isMoveVertical :: Position -> Position -> Bool
isMoveVertical src dest = (fst src == fst dest)

isMoveHorizontal :: Position -> Position -> Bool
isMoveHorizontal src dest = (snd src == snd dest)

returnPath :: Position -> Position -> [Position]
returnPath src dest
  | isMoveVertical src dest = drop 1 (zip [(fst src), (fst src) .. (fst src)] [(snd src) .. (snd dest)])
  | isMoveHorizontal src dest = drop 1 (zip [(fst src) .. (fst dest)] [(snd src), (snd src) .. (snd src)])
  | otherwise = [(0,0)]

isPathEmpty :: GameState -> Position -> Position -> Bool
isPathEmpty st src dst = not $ elem False (map (isTileEmpty st) (returnPath src dst))

isTileEmpty :: GameState -> Position -> Bool
isTileEmpty st pos = (((currState st) !! (snd pos)) !! (fst pos) == E)

positionToTile :: GameState -> Position -> Tile
positionToTile st pos = (((currState st) !! (snd pos)) !! (fst pos))

updateState :: GameState -> Position -> Position -> GameState
updateState st src dst = st {currState = newB,nextPlayer = O}
  where tile = positionToTile st src
        b = currState st
        newB = if (isMoveVertical src dst) then executeVerticalMove b tile src dst else executeHorizontalMove b tile src dst

executeHorizontalMove :: [[Tile]] -> Tile -> Position -> Position -> [[Tile]]
executeHorizontalMove b sq src dst =
  let tempRow = updateRow (b !! (snd src)) src E
      row = updateRow tempRow src sq
  in (take ((snd src)-1) b ++ [row] ++ drop (snd src) b)

executeVerticalMove :: [[Tile]] -> Tile -> Position -> Position -> [[Tile]]
executeVerticalMove b sq src dst = if (snd src > snd dst)
  then switchRowsInBoard b dstRow srcRow dst src
  else switchRowsInBoard b srcRow dstRow src dst
  where srcRow = updateRow (b !! (snd src)) src E
        dstRow = updateRow (b !! (snd dst)) src sq

updateRow :: [Tile] -> Position -> Tile -> [Tile]
updateRow row pos sq = take ((fst pos) - 1) row ++ [sq] ++ drop (fst pos) row

switchRowsInBoard :: [[Tile]] -> [Tile] -> [Tile] -> Position -> Position -> [[Tile]]
switchRowsInBoard b fstRow sndRow fstPos sndPos =
  take ((snd fstPos)-1) b ++ [fstRow] ++ (drop ((snd fstPos)+1) (take ((snd sndPos)-1) b)) ++ [sndRow] ++ drop (snd sndPos) b
