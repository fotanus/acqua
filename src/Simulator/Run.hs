module Simulator.Run where

import Logger
import AcquaIR.Language
import Simulator.Acqua
import Simulator.ProcessingUnit
import Simulator.Rules
import Simulator.Rules.Base

run :: Program -> Int -> String
run prog pus_n = step (newAcqua prog pus_n)

runMap :: Program -> Int -> String -> [String] -> String
runMap prog pus_n varName params = step (newAcquaMap prog pus_n varName params)

applyRules :: [Rule] -> Acqua -> Acqua
applyRules [] a = a
applyRules (f:fs) a = applyRules fs (f a)

step :: Acqua -> String
step acqua = _step (applyRules rules acqua) acqua

_step :: Acqua -> Acqua -> String
_step (Acqua _ _ pus _ True _) _ = "Finished!\n" ++ (showAcquaResult pus)
_step acqua acqua'= -- traceAcqua acqua $
  if acqua == acqua'
    then error $ traceAcqua acqua "Cannot give a step!\n"
    else trace ("----") $ _step (applyRules rules (unlockAll acqua)) acqua
