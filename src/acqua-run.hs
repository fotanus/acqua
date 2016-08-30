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
      if (length args) < 4
        then
          putStrLn $ "usage: ./aqua-run number_of_pus number_of_steps_for_messages function_var_name list_of_applied_values < function.l1"
        else do
          n_pus <- return $ read (args!!0)
          msg_steps <- return $ read (args!!1)
          var_name <- return $ args!!2
          params <- return $ (drop 3 args)
          putStrLn $ run (compile ast) n_pus msg_steps var_name params
    Left errorMsg ->
      putStrLn errorMsg
