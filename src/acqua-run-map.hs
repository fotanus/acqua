module Main where
import System.Environment
import Logger

import L1.Grammar
import AcquaIR.Language
import Compiler.Compile
import Simulator.Acqua
import Simulator.ProcessingUnit
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
      n_pus <- return $ read (args!!0)
      var_name <- return $ args!!1
      params <- return $ (drop 2 args)
      putStrLn $ runMap (compile ast) n_pus var_name params
    Left errorMsg ->
      putStrLn errorMsg
  where
    runMap :: Program -> Int -> String -> [String] -> String
    runMap prog pus_n varName params = step (newAcquaMap prog pus_n varName params)

    applyRules :: [Rule] -> Acqua -> Acqua
    applyRules [] a = a
    applyRules (f:fs) a = applyRules fs (f a)

    step :: Acqua -> String
    step acqua = _step (applyRules rules acqua) acqua

    _step :: Acqua -> Acqua -> String
    _step (Acqua _ _ pus _ True _) _ = "Finished!\n" ++ (acquaResultMap pus)
    _step acqua acqua'= -- traceAcqua acqua $
      if acqua == acqua'
        then error $ traceAcqua acqua "Cannot give a step!\n"
        else trace ("----") $ _step (applyRules rules (unlockAll acqua)) acqua
