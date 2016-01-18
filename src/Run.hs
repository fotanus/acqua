module Main where

import L1.Grammar
import AcquaIR.Compile
import Simulator.Run

main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn $ run (compile ast)
    Left errorMsg ->
      putStrLn errorMsg
