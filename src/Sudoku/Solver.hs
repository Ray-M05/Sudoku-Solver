module Sudoku.Solver
  ( solveOne
  , solveAll
  ) where

import Sudoku.Types
import Sudoku.Logic (isValid, validMove, findEmpty, setCell)

-- Devuelve una sola solucion (si existe)
solveOne :: Grid -> Maybe Grid
solveOne g =
  case solveAll g of
    (x:_) -> Just x
    []    -> Nothing

-- Devuelve todas las soluciones posible
solveAll :: Grid -> [Grid]
solveAll g
  | not (isValid g) = []
  | otherwise =
      case findEmpty g of
        Nothing   -> [g]  
        Just pos  ->
          [ sol
          | v <- [1..9]
          , validMove g pos v
          , let g' = setCell g pos (Just v)
          , sol <- solveAll g'
          ]
