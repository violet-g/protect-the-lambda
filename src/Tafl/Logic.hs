{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (
  move
  ) where

import Tafl.Core
import Data.Char
import Data.List

-- | Custom data type to represent a coordinate
type Position = (Int, Int)

-- | Execute a movement
move :: GameState -> String -> String -> Either TaflError GameState
move st src dst
  | isValidMove st source destination = Right $ changeTurn (checkForEnemiesToCapture (updateState st source destination) destination)
  | otherwise = Left $ InvalidMove
  where source = strToPos src
        destination = strToPos dst

-- | Convert input strings to positions
strToPos :: String -> Position
strToPos str = ((ord (head str) - 96), (ord (head (tail str)) - 48))

-- | Convert position
positionToTile :: GameState -> Position -> Tile
positionToTile st pos = (((currState st) !! (10-(snd pos)-1)) !! ((fst pos)-1))

-- | Convert a tuple to position
tupleToPos :: (Int, Int) -> Position
tupleToPos (a,b) = (a,b) :: Position

-- | Check if a move is valid
isValidMove :: GameState -> Position -> Position -> Bool
isValidMove st src dst = (not $ isMiddlePiece dst)  && (not $ isCorner dst) && (not $ isOutOfBounds src) && (not $ isOutOfBounds dst) && (not $ src == dst) && ((isMoveHorizontal src dst) || (isMoveVertical src dst)) && (isPathClear st src dst) && (not $ isTileEmpty st src) && (isTileOwned st (positionToTile st src))


-- Tile Checks

isCorner :: Position -> Bool
isCorner pos = pos == (1,1) || pos == (1,9) || pos == (9,1) || pos == (9,9)

isMiddlePiece :: Position -> Bool
isMiddlePiece pos = pos == (5,5)

isOutOfBounds :: Position -> Bool
isOutOfBounds pos = fst pos < 1 || fst pos > 9 || snd pos < 1 || snd pos > 9

isTileEmpty :: GameState -> Position -> Bool
isTileEmpty st pos = (((currState st) !! (10-(snd pos)-1)) !! ((fst pos)-1) == E)

isTileOwned :: GameState -> Tile -> Bool
isTileOwned st tile
  | ((player st) == Attacker && (tile == O)) = True
  | ((player st) == Defender && ((tile == G) || tile == L)) = True
  | otherwise = False

isMiddleEmpty :: GameState -> Bool
isMiddleEmpty st = (((currState st) !! 4) !! 4) == E

isLambda :: GameState -> Position -> Bool
isLambda st pos 
  | (positionToTile st pos) == L = True
  | otherwise = False


-- Execute Movement


-- | check if your path to destination is clear
isPathClear :: GameState -> Position -> Position -> Bool
isPathClear st src dst = not $ elem False (map (isTileEmpty st) (returnPath src dst))

-- | Get path
returnPath :: Position -> Position -> [Position]
returnPath src dest
  | isMoveVertical src dest = drop 1 (zip [(fst src), (fst src) .. (fst src)] [(snd src) .. (snd dest)])
  | isMoveHorizontal src dest = drop 1 (zip [(fst src) .. (fst dest)] [(snd src), (snd src) .. (snd src)])
  | otherwise = [(0,0)]

-- | Check direction of movement
isMoveHorizontal :: Position -> Position -> Bool
isMoveHorizontal src dest = (snd src == snd dest)

isMoveVertical :: Position -> Position -> Bool
isMoveVertical src dest = (fst src == fst dest)

-- | Move in specified direction
executeHorizontalMove :: [[Tile]] -> Tile -> Position -> Position -> [[Tile]]
executeHorizontalMove b sq src dst =
  let tempRow = updateRow (b !! (10-(snd src)-1)) src E
      row = updateRow tempRow dst sq
  in (take (10 - (snd src)-1) b ++ [row] ++ drop (10-(snd src)) b)

executeVerticalMove :: [[Tile]] -> Tile -> Position -> Position -> [[Tile]]
executeVerticalMove b sq src dst = if (snd src < snd dst)
  then switchRowsInBoard b dstRow srcRow dst src
  else switchRowsInBoard b srcRow dstRow src dst
  where srcRow = updateRow (b !! (10-(snd src)-1)) src E
        dstRow = updateRow (b !! (10-(snd dst)-1)) src sq

-- | Register the new position on the board
updateState :: GameState -> Position -> Position -> GameState
updateState st src dst = st {currState = newB}
  where tile = positionToTile st src
        b = currState st
        newB = if (isMoveVertical src dst) then executeVerticalMove b tile src dst else executeHorizontalMove b tile src dst

updateRow :: [Tile] -> Position -> Tile -> [Tile]
updateRow row pos sq = take ((fst pos) - 1) row ++ [sq] ++ drop (fst pos) row

switchRowsInBoard :: [[Tile]] -> [Tile] -> [Tile] -> Position -> Position -> [[Tile]]
switchRowsInBoard b fstRow sndRow fstPos sndPos =
  take (10-(snd fstPos)-1) b ++ [fstRow] ++ (drop (10-(snd fstPos)) (take (10-(snd sndPos)-1) b)) ++ [sndRow] ++ drop (10-(snd sndPos)) b


-- Potentially capture enemies

getAdjacentPositions :: GameState -> Position -> [Position]
getAdjacentPositions st pos = filter (\x -> not $ isOutOfBounds x) (map tupleToPos [(fst pos, (snd pos)+1), ((fst pos)+1, snd pos), (fst pos, (snd pos)-1), ((fst pos)-1, snd pos)])

isEnemy :: GameState -> Position -> Bool
isEnemy st pos
  | (player st) == Attacker && ((positionToTile st pos) == L || (positionToTile st pos) == G) = True
  | (player st) == Defender && ((positionToTile st pos) == O) = True
  | otherwise = False

isAlly :: GameState -> Position -> Bool
isAlly st pos 
  | (player st) == Defender && ((positionToTile st pos) == L || (positionToTile st pos) == G) = True
  | (player st) == Attacker && ((positionToTile st pos) == O) = True
  | otherwise = False

-- | Check for specific cases for middle and corner
isMiddleAdjacent :: GameState -> Position -> Bool
isMiddleAdjacent st pos = or (map isMiddlePiece (getAdjacentPositions st pos))

isCornerAdjacent :: GameState -> Position -> Bool
isCornerAdjacent st pos = or (map isCorner (getAdjacentPositions st pos))

opposingMiddleIsAlly :: GameState -> Position -> Bool
opposingMiddleIsAlly st pos = 
  let enemyIndex = mod ((head (elemIndices (strToPos "e5") (getAdjacentPositions st pos)))+2) 4 -- index of middle piece in surrounding pieces list
  in isAlly st ((getAdjacentPositions st pos) !! enemyIndex)

opposingCornerIsAlly :: GameState -> Position -> Bool
opposingCornerIsAlly st pos =
   let surroundings = map tupleToPos [(fst pos, (snd pos)+1), ((fst pos)+1, snd pos), (fst pos, (snd pos)-1), ((fst pos)-1, snd pos)] 
       cornerCoord = head (filter isCorner surroundings)
       enemyIndex = mod ((head $ elemIndices cornerCoord surroundings)+2) 4
   in isAlly st (surroundings !! enemyIndex)

surroundedByTwoAllies :: GameState -> Position -> Bool
surroundedByTwoAllies st pos 
  | isOutOfBounds (fst pos, (snd pos)+1) || isOutOfBounds (fst pos, (snd pos)-1) = False
  | (isAlly st (fst pos, (snd pos)+1)) && (isAlly st (fst pos, (snd pos)-1)) = True
  | isOutOfBounds ((fst pos)+1, snd pos) || isOutOfBounds ((fst pos)-1, snd pos) = False
  | (isAlly st ((fst pos)+1, snd pos)) && (isAlly st ((fst pos)-1, snd pos)) = True
  | otherwise = False

checkForEnemiesToCapture :: GameState -> Position -> GameState
checkForEnemiesToCapture st pos
  | (length coordinatesToCapture) == 0 = st
  | otherwise = newState 
  where coordinatesToCapture = (filter (isCaptured st) (filter (isEnemy st) (getAdjacentPositions st pos))) 
        newState = recursiveCapture st coordinatesToCapture

isCaptured :: GameState -> Position -> Bool
isCaptured st pos
  -- guards and objects pieces
  | not (isLambda st pos) && surroundedByTwoAllies st pos = True
  | not (isLambda st pos) && isMiddleAdjacent st pos && opposingMiddleIsAlly st pos && isMiddleEmpty st = True 
  | not (isLambda st pos) && isCornerAdjacent st pos && opposingCornerIsAlly st pos = True
  -- lambda
  | isLambda st pos && isMiddlePiece pos && and (map (isAlly st) (getAdjacentPositions st pos)) = True -- is captured by 4 in middle
  | isLambda st pos && isMiddleAdjacent st pos && and (map (isAlly st) (filter (\x -> not $ isMiddlePiece x) (getAdjacentPositions st pos))) && isMiddleEmpty st = True -- is captured by 3 next to middle
  | isLambda st pos && not (isMiddleAdjacent st pos) && surroundedByTwoAllies st pos && (not $ isMiddlePiece pos) = True
  | isLambda st pos && isCornerAdjacent st pos && opposingCornerIsAlly st pos = True
  | otherwise = False

-- | Register the capturing of the piece(s)
recursiveCapture :: GameState -> [Position] -> GameState
recursiveCapture st [] = st
recursiveCapture st (x:xs) = recursiveCapture (capturePiece st x) xs

capturePiece :: GameState -> Position -> GameState
capturePiece st pos =
  st {currState = newB} 
  where newRow = updateRow ((currState st) !! (10-snd pos-1)) pos E
        newB = take (10-(snd pos)-1) (currState st) ++ [newRow] ++ drop (10-(snd pos)) (currState st)

-- | Switch turns
changeTurn :: GameState -> GameState
changeTurn st = let newPlayer = if (player st) == Attacker then Defender else Attacker 
                in st {player = newPlayer }

