module Sudoku.CLI
  ( runCLI
  ) where

import Sudoku.Types
import Sudoku.Parser (parseGrid, readGridFromFile)
import Sudoku.Pretty (prettyGrid)
import Sudoku.Logic (validMove, isValid, isSolved, clearCell, setCell)
import Sudoku.Solver (solveOne)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

data AppState = AppState
  { currentGrid :: Grid
  , history     :: [Grid]
  }

runCLI :: IO ()
runCLI = do
  putStrLn "=== Sudoku en Haskell ==="
  putStrLn "Introduce la ruta de un archivo con el sudoku inicial"
  putStrLn "(o pulsa ENTER para usar un ejemplo integrado):"
  putStr "> "
  hFlush stdout
  path <- getLine
  eGrid <-
    if null path
      then pure (parseGrid examplePuzzle)
      else readGridFromFile path
  case eGrid of
    Left err -> putStrLn ("Error al cargar el sudoku: " ++ err)
    Right g  ->
      if isValid g
        then do
          putStrLn "Sudoku valido cargado. Escribe 'help' para ver comandos."
          loop (AppState g [])
        else do
          putStrLn "El sudoku del archivo es invalido. Revisa filas/columnas/bloques."
          putStrLn "No se puede continuar con este tablero."


loop :: AppState -> IO ()
loop st = do
  putStrLn ""
  putStrLn (prettyGrid (currentGrid st))
  putStrLn ""
  putStr "> "
  hFlush stdout
  line <- getLine
  case words line of
    [] -> loop st

    ("help":_) -> do
      printHelp
      loop st

    ("quit":_) -> do
      putStrLn "Hasta luego!"

    ("set":r:c:v:_) ->
      case (readMaybe r, readMaybe c, readMaybe v) of
        (Just ri, Just ci, Just vi) ->
          handleSet st (ri,ci,vi)
        _ -> do
          putStrLn "Uso: set <fila 1-9> <columna 1-9> <valor 1-9>"
          loop st

    ("clear":r:c:_) ->
      case (readMaybe r, readMaybe c) of
        (Just ri, Just ci) ->
          handleClear st (ri,ci)
        _ -> do
          putStrLn "Uso: clear <fila 1-9> <columna 1-9>"
          loop st

    ("solve":_) ->
      handleSolve st

    ("hint":_) ->
      handleHint st

    ("check":_) -> do
      handleCheck st
      loop st

    ("undo":_) ->
      handleUndo st

    _ -> do
      putStrLn "Comando no reconocido. Escribe 'help' para ver opciones."
      loop st

printHelp :: IO ()
printHelp = do
  putStrLn "Comandos disponibles:"
  putStrLn "  set f c v   - poner valor v (1-9) en fila f, columna c (1-9)"
  putStrLn "  clear f c   - vaciar la casilla en fila f, columna c"
  putStrLn "  solve       - resolver el sudoku actual"
  putStrLn "  hint        - dar una pista (si es posible)"
  putStrLn "  check       - comprobar si el tablero es valido / resuelto"
  putStrLn "  undo        - deshacer el ultimo cambio"
  putStrLn "  help        - mostrar esta ayuda"
  putStrLn "  quit        - salir"

handleSet :: AppState -> (Int,Int,Int) -> IO ()
handleSet st (ri,ci,vi)
  | any (\x -> x < 1 || x > 9) [ri,ci,vi] = do
      putStrLn "Fila, columna y valor deben estar entre 1 y 9."
      loop st
  | otherwise = do
      let r = ri - 1
          c = ci - 1
          v = vi
          g = currentGrid st
      if validMove g (r,c) v
        then do
          let g' = setCell g (r,c) (Just v)
          loop st { currentGrid = g', history = g : history st }
        else do
          putStrLn "Movimiento invalido (viola fila/columna/bloque, o casilla no vacia)."
          loop st

handleClear :: AppState -> (Int,Int) -> IO ()
handleClear st (ri,ci)
  | any (\x -> x < 1 || x > 9) [ri,ci] = do
      putStrLn "Fila y columna deben estar entre 1 y 9."
      loop st
  | otherwise = do
      let r = ri - 1
          c = ci - 1
          g = currentGrid st
          g' = clearCell g (r,c)
      loop st { currentGrid = g', history = g : history st }

handleSolve :: AppState -> IO ()
handleSolve st = do
  let g = currentGrid st
  case solveOne g of
    Nothing -> do
      putStrLn "No se ha encontrado ninguna solucion (o el tablero es invalido)."
      loop st
    Just sol -> do
      putStrLn "Solucion encontrada:"
      putStrLn (prettyGrid sol)
      loop st { currentGrid = sol, history = g : history st }

handleHint :: AppState -> IO ()
handleHint st = do
  let g = currentGrid st
  case solveOne g of
    Nothing -> putStrLn "No puedo dar pistas: el sudoku parece no tener solucion."
    Just sol ->
      case firstDifference g sol of
        Nothing -> putStrLn "El sudoku ya esta resuelto."
        Just (r,c,v) ->
          putStrLn $
            "Pista: fila " ++ show (r+1) ++
            ", columna " ++ show (c+1) ++
            " deberia ser " ++ show v
  loop st

handleCheck :: AppState -> IO ()
handleCheck st = do
  let g = currentGrid st
  if isValid g
    then if isSolved g
          then putStrLn "El tablero es valido y esta completamente resuelto. ¡Bien!"
          else putStrLn "El tablero es valido pero aun no esta completo."
    else putStrLn "El tablero tiene conflictos (fila/columna/bloque)."

handleUndo :: AppState -> IO ()
handleUndo st =
  case history st of
    [] -> do
      putStrLn "No hay movimientos que deshacer."
      loop st
    (prev:rest) -> do
      putStrLn "Deshaciendo el ultimo movimiento."
      loop st { currentGrid = prev, history = rest }

-- Primera casilla vacía en g1 que está rellena en g2
firstDifference :: Grid -> Grid -> Maybe (Int, Int, Value)
firstDifference g1 g2 = go 0 0
  where
    go r c
      | r >= gridSize = Nothing
      | c >= gridSize = go (r+1) 0
      | otherwise =
          let cell1 = g1 !! r !! c
              cell2 = g2 !! r !! c
          in case (cell1, cell2) of
               (Nothing, Just v) -> Just (r,c,v)
               _                 -> go r (c+1)

-- Un sudoku de ejemplo hardcodeado
examplePuzzle :: String
examplePuzzle = unlines
  [ "53..7...."
  , "6..195..."
  , ".98....6."
  , "8...6...3"
  , "4..8.3..1"
  , "7...2...6"
  , ".6....28."
  , "...419..5"
  , "....8..79"
  ]
