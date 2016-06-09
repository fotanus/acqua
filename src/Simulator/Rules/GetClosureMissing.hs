module Simulator.Rules.GetClosureMissing where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

getClosureMissing :: Rule
getClosureMissing (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetClosureMissing pus) i f s
  where
    stepGetClosureMissing pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureMissing x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureMissing " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PointerV pointer = getVal pu x
            ClosureV closure = Heap.lookupPt pointer (heap pu)
            missing = (paramMissing closure)
            pu' = (setVal pu n (NumberV missing)) { PU.commands = cs, locked = True }
        _ -> pu
