module Sudoku.Pretty
  ( prettyGrid
  ) where

import Sudoku.Types
import Data.List (intercalate)

showCell :: Cell -> String
showCell Nothing  = "."
showCell (Just v) = show v

chunk3 :: [a] -> [[a]]
chunk3 [] = []
chunk3 xs = take 3 xs : chunk3 (drop 3 xs)

showRow :: Row -> String
showRow row =
  let groups     = take 3 (chunk3 row)  -- deberian ser exactamente 3
      groupStrs  = map (unwords . map showCell) groups
  in "| " ++ intercalate " | " groupStrs ++ " |"

separator :: String
separator = "+-------+-------+-------+"

prettyGrid :: Grid -> String
prettyGrid rows =
  let rendered =
        concatMap renderRow (zip [0..] rows)

      renderRow (i, row) =
        let base  = [showRow row]
            extra = if i `mod` blockSize == blockSize - 1
                      then [separator]
                      else []
        in base ++ extra

  in unlines (separator : rendered)
