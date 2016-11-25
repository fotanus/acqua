module Simulator.Rules.GetNPU where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value

import Simulator.Rules.Base

getNPU :: Rule
getNPU acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepGetNPU (processingUnits acqua)
    stepGetNPU pu =
      case (PU.commands pu, PU.canExecuteCmds pu) of
        ((GetNPU x1):cs, True) -> trace ((show (PU.puId pu)) ++ ": getNPU with result " ++ (show val)) pu'
          where
            val = length (processingUnits acqua)
            pu' = (setVal pu x1 (NumberV val)) { PU.commands = cs, locked = True }
        _ -> pu

