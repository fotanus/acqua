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
          putStrLn $ "usage: ./aqua-run opt_level npus nstepsm var val < function.l1\n\n  opt_level: optimization level between 0 and 3\n  npus: number of processing units\n  nstepsm: number of steps a message takes to reach another pu (for hierarchical crossbar), 0 for disabled\n  var: variable name with starting value\n  val: Starting values for var.\n\nFor multiple starting jobs, add more values as extra arguments"
        else do
          opt <- return $ read (args!!0)
          n_pus <- return $ read (args!!1)
          msg_steps <- return $ read (args!!2)
          var_name <- return $ args!!3
          params <- return $ (drop 4 args)
          putStrLn $ run (compile ast opt) opt n_pus msg_steps var_name params
    Left errorMsg ->
      putStrLn errorMsg
