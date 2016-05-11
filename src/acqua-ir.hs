module Main where

import L1.Grammar
import UL1.FromL1
import AcquaIR.Language
import Compiler.Compile

-- Prints the acqua IR for a given L1 program
main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn (printProgram (compile (fromL1 ast)))
    Left errorMsg ->
      putStrLn errorMsg
