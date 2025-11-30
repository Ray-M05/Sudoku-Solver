module Sudoku.Parser
  ( parseGrid
  , readGridFromFile
  ) where

import Sudoku.Types
import Data.Char (digitToInt)

-- Parsea un sudoku desde un String (9 líneas de 9 caracteres).
-- Caracteres válidos:
--   '.' o '0' -> casilla vacía
--   '1'..'9'  -> valor
parseGrid :: String -> Either String Grid
parseGrid input =
  let ls = filter (not . null) (lines input)
  in if length ls /= gridSize
       then Left ("Se esperaban " ++ show gridSize ++ " lineas no vacias")
       else mapM parseRow (zip [1..] ls)

parseRow :: (Int, String) -> Either String Row
parseRow (rowNum, str) =
  if length str /= gridSize
    then Left ("Fila " ++ show rowNum ++
               ": se esperaban " ++ show gridSize ++ " caracteres")
    else mapM (parseCell rowNum) (zip [1..] str)

parseCell :: Int -> (Int, Char) -> Either String Cell
parseCell rowNum (colNum, ch)
  | ch == '.' || ch == '0' = Right Nothing
  | ch >= '1' && ch <= '9' = Right (Just (digitToInt ch))
  | ch == ' '              = Right Nothing  -- permitir espacios
  | otherwise =
      Left ("Caracter invalido '" ++ [ch] ++
            "' en fila " ++ show rowNum ++
            ", columna " ++ show colNum)

readGridFromFile :: FilePath -> IO (Either String Grid)
readGridFromFile path = do
  content <- readFile path
  pure (parseGrid content)
