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
      msg_steps <- return $ read (args!!1)
      var_name <- return $ args!!2
      params <- return $ (drop 3 args)
      putStrLn $ runMap (compile ast) n_pus msg_steps var_name params
    Left errorMsg ->
      putStrLn errorMsg
