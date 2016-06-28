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
      n_pus <- return $ read (args!!0)
      var_name <- return $ args!!1
      params <- return $ (drop 2 args)
      putStrLn $ runMap (compile ast) n_pus var_name params
    Left errorMsg ->
      putStrLn errorMsg
