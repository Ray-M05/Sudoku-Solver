module Main (main) where

import Test.Hspec
import Sudoku.Parser (parseGrid)
import Sudoku.Logic  (isValid, isSolved)
import Sudoku.Solver (solveOne)

-- Helper para cargar un tablero desde un archivo
loadPuzzle :: FilePath -> IO String
loadPuzzle path = readFile path

main :: IO ()
main = hspec $ do

  describe "Sudoku.Parser + Logic" $ do

    it "parsea y considera valido el tablero easy" $ do
      txt <- loadPuzzle "test/easy.txt"
      case parseGrid txt of
        Left err   -> expectationFailure err
        Right grid -> isValid grid `shouldBe` True

    it "detecta como invalido el tablero invalid" $ do
      txt <- loadPuzzle "test/invalid.txt"
      case parseGrid txt of
        Left err   -> expectationFailure err
        Right grid -> isValid grid `shouldBe` False

    it "reconoce done como tablero ya resuelto y valido" $ do
      txt <- loadPuzzle "test/done.txt"
      case parseGrid txt of
        Left err   -> expectationFailure err
        Right grid -> do
          isValid  grid `shouldBe` True
          isSolved grid `shouldBe` True

  describe "Sudoku.Solver" $ do

    it "resuelve el tablero medium" $ do
      txt <- loadPuzzle "test/medium.txt"
      case parseGrid txt of
        Left err   -> expectationFailure err
        Right grid ->
          case solveOne grid of
            Nothing     -> expectationFailure "No se encontro solucion para medium"
            Just solved -> isSolved solved `shouldBe` True

    it "resuelve el tablero hard" $ do
      txt <- loadPuzzle "test/hard.txt"
      case parseGrid txt of
        Left err   -> expectationFailure err
        Right grid ->
          case solveOne grid of
            Nothing     -> expectationFailure "No se encontro solucion para hard"
            Just solved -> isSolved solved `shouldBe` True
