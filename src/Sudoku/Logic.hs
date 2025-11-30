module Sudoku.Logic
  ( rowAt
  , colAt
  , blockAt
  , isValid
  , isSolved
  , validMove
  , setCell
  , clearCell
  , findEmpty
  ) where

import Sudoku.Types
import Data.Maybe (catMaybes, isJust)
import Data.List (nub)

rowAt :: Grid -> Int -> Row
rowAt g r = g !! r

colAt :: Grid -> Int -> [Cell]
colAt g c = map (!! c) g

blockAt :: Grid -> Position -> [Cell]
blockAt g (r,c) =
  [ g !! r' !! c'
  | r' <- [br .. br + blockSize - 1]
  , c' <- [bc .. bc + blockSize - 1]
  ]
  where
    br = (r `div` blockSize) * blockSize
    bc = (c `div` blockSize) * blockSize

noDupes :: Eq a => [a] -> Bool
noDupes xs = length xs == length (nub xs)

rowsValid :: Grid -> Bool
rowsValid g = all (noDupes . catMaybes) g

colsValid :: Grid -> Bool
colsValid g =
  all (noDupes . catMaybes)
      [ colAt g c | c <- [0 .. gridSize - 1] ]

blocksValid :: Grid -> Bool
blocksValid g =
  all (noDupes . catMaybes)
      [ blockAt g (r,c)
      | r <- [0, blockSize .. gridSize - 1]
      , c <- [0, blockSize .. gridSize - 1]
      ]

isValid :: Grid -> Bool
isValid g = rowsValid g && colsValid g && blocksValid g

isSolved :: Grid -> Bool
isSolved g = isValid g && all (all isJust) g

setCell :: Grid -> Position -> Cell -> Grid
setCell g (r,c) val =
  take r g ++
  [row'] ++
  drop (r + 1) g
  where
    row  = g !! r
    row' = take c row ++ [val] ++ drop (c + 1) row

clearCell :: Grid -> Position -> Grid
clearCell g pos = setCell g pos Nothing

-- Comprueba si es valido poner v en (r,c), suponiendo que la casilla está vacía.
validMove :: Grid -> Position -> Value -> Bool
validMove g (r,c) v =
  inRange && cellEmpty && notInRow && notInCol && notInBlock
  where
    inRange = r >= 0 && r < gridSize
           && c >= 0 && c < gridSize
           && v >= 1 && v <= 9

    cell = g !! r !! c
    cellEmpty = cell == Nothing

    notInRow   = Just v `notElem` rowAt g r
    notInCol   = Just v `notElem` colAt g c
    notInBlock = Just v `notElem` blockAt g (r,c)

findEmpty :: Grid -> Maybe Position
findEmpty g =
  case [ (r,c)
       | r <- [0 .. gridSize - 1]
       , c <- [0 .. gridSize - 1]
       , g !! r !! c == Nothing
       ] of
    []    -> Nothing
    (p:_) -> Just p
