module Sudoku.Types
  ( Value
  , Cell
  , Row
  , Grid
  , Position
  , gridSize
  , blockSize
  ) where

type Value = Int
type Cell  = Maybe Value
type Row   = [Cell]
type Grid  = [Row]
type Position = (Int, Int)   -- (fila, columna), índices 0..8

gridSize :: Int
gridSize = 9

blockSize :: Int
blockSize = 3
