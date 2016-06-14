module Simulator.Rules.GetClosureMissing where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

getClosureMissing :: Rule
getClosureMissing acqua  =
    acqua { processingUnits = newPus }
  where
    newPus = map stepGetClosureMissing (processingUnits acqua)
    stepGetClosureMissing pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureMissing x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureMissing " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PointerV pointer = getVal pu x
            ClosureV closure = Heap.lookupPt pointer (heap pu)
            missing = (paramMissing closure)
            pu' = (setVal pu n (NumberV missing)) { PU.commands = cs, locked = True }
        _ -> pu
