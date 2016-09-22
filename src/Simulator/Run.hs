module Simulator.Run where

import Logger
import AcquaIR.Language
import Simulator.Acqua
import Simulator.Stats
import Simulator.Rules
import Simulator.Rules.Base

run :: Program -> Int -> Int -> String -> [String] -> String
run prog pus_n steps_to_propagate varName params = step (newAcquaMap prog pus_n steps_to_propagate varName params)

applyRules :: [Rule] -> Acqua -> Acqua
applyRules [] a = a
applyRules (f:fs) a = applyRules fs (f a)

step :: Acqua -> String
step acqua = _step (applyRules rules acqua) acqua

_step :: Acqua -> Acqua -> String
_step acqua _ | (finishFlag acqua) == True = "Finished!\n" ++ (showAcquaResult acqua)
_step acqua acqua'= -- traceAcqua acqua $
  if acqua == acqua'
    then error $ traceAcqua acqua "Cannot give a step!\n"
    else trace ("----") $ _step (trackStats (applyRules rules (unlockAndUnstallAll acqua))) acqua
