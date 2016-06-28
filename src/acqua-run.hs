module Main where

import System.Environment
import L1.Grammar
import Compiler.Compile
import Simulator.Run

-- Run a L1 program in a simulation
main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      args <- getArgs
      n_pus <- return $ read (head args)
      putStrLn $ run (compile ast) n_pus
    Left errorMsg ->
      putStrLn errorMsg
