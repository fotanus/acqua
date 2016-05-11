module Main where
import System.Environment
import Logger
import Text.Show.Pretty

import L1.Grammar
import UL1.FromL1
import AcquaIR.Language
import Compiler.Compile
import Simulator.Acqua
import Simulator.Rules
import Simulator.Rules.Base

-- Run a L1 program in a simulation
main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      args <- getArgs
      n_pus <- return $ read (head args)
      putStrLn $ run (compile (fromL1 ast)) n_pus
    Left errorMsg ->
      putStrLn errorMsg
  where
    run :: Program -> Int -> String
    run program pus_n = step (newAcqua program pus_n)

    applyRules :: [Rule] -> Acqua -> Acqua
    applyRules [] a = a
    applyRules (f:fs) a = applyRules fs (f a)

    step :: Acqua -> String
    step acqua = _step (applyRules rules acqua) acqua

    _step :: Acqua -> Acqua -> String
    _step (Acqua _ _ pus _ True _) _ = "Finished!\n" ++ (ppShow (head pus))
    _step acqua acqua'=
      if acqua == acqua'
        then error ("Cannot give a step!\n" ++ (ppShow acqua))
        else trace ("----") $ _step (applyRules rules (unlockAll acqua)) acqua
