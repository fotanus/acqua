module Simulator.Run where

import Logger
import AcquaIR.Language
import Simulator.Acqua
import Simulator.Stats
import Simulator.Rules
import Simulator.Rules.Base
import Simulator.GarbageCollector

run :: Program -> Int -> Int -> String -> [String] -> String
run prog pus_n steps_to_propagate varName params = step (newAcqua prog pus_n steps_to_propagate varName params)

applyRules :: [Rule] -> Acqua -> Acqua
applyRules [] a = a
applyRules (f:fs) a = applyRules fs (f a)

step :: Acqua -> String
step acqua = _step (applyRules rules acqua) acqua

_step :: Acqua -> Acqua -> String
_step acqua _ | (finishFlag acqua) == True = "Finished!\n" ++ (showAcquaResult acqua)
_step acqua acqua'= -- traceAcqua acqua $
  if (processingUnits acqua) == (processingUnits acqua') && (interconnection acqua) == (interconnection acqua') && (queue acqua) == (queue acqua')
    then error $ traceAcqua acqua "Cannot give a step!\n"
    else trace ("----") $ _step (trackStats (garbageCollector (applyRules rules (unlockAndUnstallAll acqua)))) acqua
