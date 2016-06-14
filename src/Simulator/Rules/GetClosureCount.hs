module Simulator.Rules.GetClosureCount where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

getClosureCount :: Rule
getClosureCount (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetClosureCount pus) i f s
  where
    stepGetClosureCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PointerV pointer = getVal pu x
            ClosureV closure = Heap.lookupPt pointer (heap pu)
            count = (paramCount closure)
            pu' = (setVal pu n (NumberV count)) { PU.commands = cs, locked = True }
        _ -> pu
