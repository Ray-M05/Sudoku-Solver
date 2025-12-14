module Sudoku.GUI
  ( runGUI
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Sudoku.Types
import Sudoku.Parser (parseGrid, readGridFromFile)
import Sudoku.Logic  (validMove, clearCell, setCell, isSolved, isValid)
import Sudoku.Solver (solveOne)

-- ======================
-- Estado de la interfaz
-- ======================

data GuiState = GuiState
  { guiGrid      :: Grid
  , guiSel       :: Maybe Position
  , guiMsg       :: String
  , guiHist      :: [Grid]
  , guiLast      :: Maybe Position      -- última casilla modificada
  , guiPath      :: String              -- texto del input de ruta
  , guiFocusPath :: Bool                -- ¿estamos editando la ruta?
  }

-- ======================
-- Parámetros visuales
-- ======================

cellSize :: Float
cellSize = 60

gridPixelSize :: Float
gridPixelSize = cellSize * fromIntegral gridSize

halfGrid :: Float
halfGrid = gridPixelSize / 2

-- desplazamos el tablero un poco hacia arriba
boardOffsetY :: Float
boardOffsetY = 80

windowWidth, windowHeight :: Int
windowWidth  = 600
windowHeight = 720

-- Botones
data ButtonName
  = BtnSolve
  | BtnHint
  | BtnCheck
  | BtnClear
  | BtnUndo
  | BtnReset
  | BtnLoad
  deriving (Eq, Show)

buttonWidth, buttonHeight :: Float
buttonWidth  = 90
buttonHeight = 32

-- nombre, texto, centro X, centro Y
buttonLayout :: [(ButtonName, String, Float, Float)]
buttonLayout =
  [ (BtnSolve, "Solve",  -200, -270)
  , (BtnHint , "Hint" ,   -70, -270)
  , (BtnCheck, "Check",    60, -270)
  , (BtnClear, "Clear",   190, -270)
  , (BtnUndo , "Undo" ,   -70, -320)
  , (BtnReset, "Reset",    60, -320)
  , (BtnLoad , "Cargar",  230, -220)
  ]

-- cuadro de texto para ruta
pathBoxWidth, pathBoxHeight :: Float
pathBoxWidth  = 340
pathBoxHeight = 32

pathBoxCenterX, pathBoxCenterY :: Float
pathBoxCenterX = -40
pathBoxCenterY = -220

-- ======================
-- Puzzle inicial
-- ======================

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

initialGrid :: Grid
initialGrid =
  case parseGrid examplePuzzle of
    Left _  -> replicate gridSize (replicate gridSize Nothing)
    Right g -> g

-- ======================
-- Arranque (playIO para poder hacer IO al cargar archivos)
-- ======================

runGUI :: IO ()
runGUI = do
  let window = InWindow "Sudoku Haskell" (windowWidth, windowHeight) (100, 100)
      bg     = white
      fps    = 30
      initState = GuiState
        { guiGrid      = initialGrid
        , guiSel       = Nothing
        , guiMsg       = "Click en una casilla; 1-9 para poner, 0/. para borrar, s/h/c/u/r para atajos"
        , guiHist      = []
        , guiLast      = Nothing
        , guiPath      = ""
        , guiFocusPath = False
        }
  playIO window bg fps initState
         (\s -> pure (drawGui s))
         handleEventIO
         (\_ s -> pure s)

-- ======================
-- Dibujo
-- ======================

drawGui :: GuiState -> Picture
drawGui st =
  Pictures
    [ drawGrid
    , drawSelection (guiSel st)
    , drawNumbers st
    , drawPathBox st
    , drawButtons
    , drawMessage (guiMsg st)
    ]

drawGrid :: Picture
drawGrid =
  Translate 0 boardOffsetY $
    Pictures (outer : hLines ++ vLines)
  where
    outer = Color black $ rectangleWire gridPixelSize gridPixelSize

    hLines =
      [ Color (if i `mod` blockSize == 0 then black else greyN 0.8) $
          line [(-halfGrid, y), (halfGrid, y)]
      | i <- [1 .. gridSize - 1]
      , let y = halfGrid - fromIntegral i * cellSize
      ]

    vLines =
      [ Color (if i `mod` blockSize == 0 then black else greyN 0.8) $
          line [(x, -halfGrid), (x, halfGrid)]
      | i <- [1 .. gridSize - 1]
      , let x = -halfGrid + fromIntegral i * cellSize
      ]

drawNumbers :: GuiState -> Picture
drawNumbers st =
  Translate 0 boardOffsetY $
    Pictures
      [ drawCellNumber r c v
      | (r, row)  <- zip [0..] (guiGrid st)
      , (c, cell) <- zip [0..] row
      , Just v    <- [cell]
      ]
  where
    textScale = 0.23
    lastPos   = guiLast st

    drawCellNumber r c v =
      let (x, y) = coordOfCell (r,c)

          isLast =
            case lastPos of
              Just (lr, lc) -> lr == r && lc == c
              Nothing       -> False

          -- verde más intenso para el último movimiento
          fillColor =
            if isLast
              then makeColorI 0 230 0 255   -- verde brillante
              else black

          outlineColor = black

          baseChar :: Color -> Picture
          baseChar col = Color col (Text (show v))

          baseOffsetX = -10
          baseOffsetY = -18

          -- varios offsets alrededor para simular borde grueso
          outlineOffsets :: [(Float, Float)]
          outlineOffsets =
            [ (baseOffsetX + ox, baseOffsetY + oy)
            | ox <- [-2,-1,0,1,2]
            , oy <- [-2,-1,0,1,2]
            , (ox,oy) /= (0,0)
            ]

          outlinePics =
            [ Translate dx dy (baseChar outlineColor)
            | (dx,dy) <- outlineOffsets
            ]

          centerPic =
            Translate baseOffsetX baseOffsetY (baseChar fillColor)

      in Translate x y $
         Scale textScale textScale $
           Pictures (outlinePics ++ [centerPic])
drawSelection :: Maybe Position -> Picture
drawSelection Nothing     = Blank
drawSelection (Just pos)  =
  Translate 0 boardOffsetY $
    let (x,y) = coordOfCell pos
    in Color (makeColorI 200 200 255 80) $
         Translate x y $
           rectangleSolid cellSize cellSize

drawButtons :: Picture
drawButtons =
  Pictures [ drawButton label cx cy | (_,label,cx,cy) <- buttonLayout ]
  where
    drawButton label cx cy =
      Pictures
        [ Translate cx cy $
            Color (light (greyN 0.9)) $
              rectangleSolid buttonWidth buttonHeight
        , Translate cx cy $
            Color black $
              rectangleWire buttonWidth buttonHeight
        , Translate (cx - 35) (cy - 7) $
            Scale 0.1 0.1 $
              Color black $
                Text label
        ]

-- cuadro de texto para ruta
drawPathBox :: GuiState -> Picture
drawPathBox st =
  Pictures
    [ Translate pathBoxCenterX pathBoxCenterY $
        Color (if guiFocusPath st then light (greyN 0.8) else light (greyN 0.9)) $
          rectangleSolid pathBoxWidth pathBoxHeight
    , Translate pathBoxCenterX pathBoxCenterY $
        Color black $
          rectangleWire pathBoxWidth pathBoxHeight
    , Translate (pathBoxCenterX - pathBoxWidth/2 + 8) (pathBoxCenterY - 7) $
        Scale 0.09 0.09 $
          Color (if null (guiPath st) then greyN 0.5 else black) $
            Text (if null (guiPath st)
                     then "Ruta del tablero (ej: test/easy.txt)"
                     else guiPath st)
    ]

drawMessage :: String -> Picture
drawMessage msg =
  Translate (-280) (-400) $
    Scale 0.13 0.13 $
      Color black $
        Text msg

-- ======================
-- Coordenadas tablero
-- ======================

coordOfCell :: Position -> (Float, Float)
coordOfCell (r,c) =
  ( (fromIntegral c - 4) * cellSize
  , (4 - fromIntegral r) * cellSize
  )

cellFromCoord :: (Float, Float) -> Maybe Position
cellFromCoord (xScreen, yScreen) =
  let y = yScreen - boardOffsetY
      x = xScreen
  in if abs x > halfGrid || abs y > halfGrid
       then Nothing
       else
         let c = floor (x / cellSize + 4.5)
             r = floor (4.5 - y / cellSize)
         in if r >= 0 && r < gridSize && c >= 0 && c < gridSize
              then Just (r,c)
              else Nothing

-- hit test del cuadro de ruta
pathBoxHit :: (Float,Float) -> Bool
pathBoxHit (mx,my) =
  let hw = pathBoxWidth  / 2
      hh = pathBoxHeight / 2
  in abs (mx - pathBoxCenterX) <= hw
     && abs (my - pathBoxCenterY) <= hh

-- ======================
-- Helpers de estado
-- ======================

setMsg :: String -> GuiState -> GuiState
setMsg m st = st { guiMsg = m }

updateGrid :: Maybe Position -> (Grid -> Grid) -> String -> GuiState -> GuiState
updateGrid mpos f msg st =
  let g' = f (guiGrid st)
  in st { guiHist = guiGrid st : guiHist st
        , guiGrid = g'
        , guiMsg  = msg
        , guiLast = mpos
        }

doUndo :: GuiState -> GuiState
doUndo st =
  case guiHist st of
    []       -> setMsg "No hay movimientos que deshacer" st
    (g:rest) -> st { guiGrid = g
                   , guiHist = rest
                   , guiMsg  = "Movimiento deshecho"
                   , guiLast = Nothing
                   }

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

dropLast :: String -> String
dropLast [] = []
dropLast xs = init xs

-- ======================
-- Eventos (IO por el load desde archivo)
-- ======================

handleEventIO :: Event -> GuiState -> IO GuiState
handleEventIO ev st =
  case ev of
    -- click izquierdo
    EventKey (MouseButton LeftButton) Down _ (mx,my) ->
      if pathBoxHit (mx,my)
        then
          -- foco en cuadro de texto
          pure st { guiFocusPath = True, guiSel = Nothing }
        else
          case clickedButton (mx,my) of
            Just BtnLoad  -> handleLoadPathIO st
            Just btnOther ->
              pure (handleButtonClick btnOther (st { guiFocusPath = False }))
            Nothing ->
              case cellFromCoord (mx,my) of
                Nothing  -> pure st { guiSel = Nothing
                                    , guiMsg = "Click fuera del tablero"
                                    , guiFocusPath = False
                                    }
                Just pos -> pure st { guiSel = Just pos
                                    , guiMsg = "Seleccionada fila "
                                               ++ show (fst pos + 1)
                                               ++ ", col "
                                               ++ show (snd pos + 1)
                                    , guiFocusPath = False
                                    }

    -- teclas de caracteres
    EventKey (Char ch) Down _ _ ->
      pure (handleChar ch st)

    -- Enter: si estamos en el cuadro de ruta, cargar
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      if guiFocusPath st
        then handleLoadPathIO st
        else pure st

    -- Backspace: borrar un carácter de la ruta si estamos en el cuadro
    EventKey (SpecialKey KeyBackspace) Down _ _ ->
      if guiFocusPath st
        then pure st { guiPath = dropLast (guiPath st) }
        else pure st

    _ -> pure st

-- teclado: routing entre cuadro de texto y sudoku
handleChar :: Char -> GuiState -> GuiState
handleChar ch st
  -- Editando la ruta
  | guiFocusPath st =
      case ch of
        '\b'   -> st { guiPath = dropLast (guiPath st) }
        '\DEL' -> st { guiPath = dropLast (guiPath st) }
        _ | ch >= ' ' && ch <= '~'
          -> st { guiPath = guiPath st ++ [ch] }
          | otherwise -> st
  -- Controlando el sudoku
  | otherwise =
      sudokuKeyHandler ch st

sudokuKeyHandler :: Char -> GuiState -> GuiState
sudokuKeyHandler ch st
  | ch >= '1' && ch <= '9' =
      case guiSel st of
        Nothing  -> setMsg "Selecciona una casilla primero" st
        Just pos ->
          let v = read [ch] :: Int
              g = guiGrid st
          in if validMove g pos v
               then updateGrid (Just pos)
                               (\g0 -> setCell g0 pos (Just v))
                               "Movimiento valido"
                               st
               else setMsg "Movimiento invalido (conflicto o casilla no vacia)" st

  | ch == '0' || ch == '.' =
      case guiSel st of
        Nothing  -> setMsg "Nada que borrar: no hay casilla seleccionada" st
        Just pos -> updateGrid (Just pos)
                               (\g0 -> clearCell g0 pos)
                               "Casilla borrada"
                               st

  | ch == 's' = handleButtonClick BtnSolve st
  | ch == 'h' = handleButtonClick BtnHint  st
  | ch == 'c' = handleButtonClick BtnCheck st
  | ch == 'u' = handleButtonClick BtnUndo  st
  | ch == 'r' = handleButtonClick BtnReset st
  | otherwise = st

clickedButton :: (Float,Float) -> Maybe ButtonName
clickedButton (mx,my) =
  let hw = buttonWidth  / 2
      hh = buttonHeight / 2
  in case [ name
          | (name,_,cx,cy) <- buttonLayout
          , abs (mx - cx) <= hw
          , abs (my - cy) <= hh
          ] of
       (b:_) -> Just b
       []    -> Nothing

handleButtonClick :: ButtonName -> GuiState -> GuiState
handleButtonClick btn st =
  case btn of
    BtnLoad  -> st  -- el load real se hace en handleLoadPathIO

    BtnClear ->
      case guiSel st of
        Nothing  -> setMsg "Selecciona una casilla para borrar" st
        Just pos -> updateGrid (Just pos)
                               (\g -> clearCell g pos)
                               "Casilla borrada"
                               st

    BtnCheck ->
      let g = guiGrid st
      in if isValid g
           then if isSolved g
                  then setMsg "Tablero valido y resuelto" st
                  else setMsg "Tablero valido pero incompleto" st
           else setMsg "El tablero tiene conflictos (fila/columna/bloque)" st

    BtnSolve ->
      let g = guiGrid st
      in case solveOne g of
           Nothing   -> setMsg "No se ha encontrado solucion (o el tablero es invalido)" st
           Just sol  -> updateGrid Nothing (const sol) "Sudoku resuelto" st

    BtnHint  ->
      let g = guiGrid st
      in case solveOne g of
           Nothing  -> setMsg "No puedo dar pistas: el sudoku parece no tener solucion" st
           Just sol ->
             case firstDifference g sol of
               Nothing         -> setMsg "El sudoku ya esta resuelto" st
               Just (r,c,v)    ->
                 updateGrid
                   (Just (r,c))
                   (\_ -> setCell g (r,c) (Just v))
                   ("Pista: fila " ++ show (r+1)
                        ++ ", col " ++ show (c+1)
                        ++ " = " ++ show v)
                   st

    BtnUndo  -> doUndo st

    BtnReset ->
      st { guiGrid      = initialGrid
         , guiSel       = Nothing
         , guiHist      = []
         , guiLast      = Nothing
         , guiMsg       = "Tablero reiniciado"
         }

-- cargar tablero desde la ruta escrita en guiPath
handleLoadPathIO :: GuiState -> IO GuiState
handleLoadPathIO st = do
  let path = guiPath st
  if null path
    then pure (setMsg "Ruta vacia" st)
    else do
      eGrid <- readGridFromFile path
      case eGrid of
        Left err ->
          pure (setMsg ("Error al cargar: " ++ err) st)
        Right g  ->
          let msg
                | isValid g && isSolved g = "Tablero cargado (valido y resuelto)"
                | isValid g               = "Tablero cargado (valido)"
                | otherwise               = "Tablero cargado (con conflictos)"
          in pure st { guiGrid      = g
                     , guiHist      = []
                     , guiSel       = Nothing
                     , guiLast      = Nothing
                     , guiMsg       = msg
                     }
