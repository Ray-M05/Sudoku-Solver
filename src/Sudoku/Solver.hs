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

-- Devuelve todas las soluciones posibles (normalmente 0 o 1 para sudokus bien formados)
solveAll :: Grid -> [Grid]
solveAll g
  | not (isValid g) = []
  | otherwise =
      case findEmpty g of
        Nothing   -> [g]   -- no hay casillas vacias: solucion encontrada
        Just pos  ->
          [ sol
          | v <- [1..9]
          , validMove g pos v
          , let g' = setCell g pos (Just v)
          , sol <- solveAll g'
          ]
