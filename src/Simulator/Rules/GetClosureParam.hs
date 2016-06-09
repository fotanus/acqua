module Simulator.Rules.GetClosureParam where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

getClosureParam :: Rule
getClosureParam (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetClosureParam pus) i f s
  where
    stepGetClosureParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureParam x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureParam " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            PointerV pointer = getVal pu x
            ClosureV closure = Heap.lookupPt pointer (heap pu)
            val = Sequence.index (params closure) i
            pu' = (setVal pu v val) { PU.commands = cs, locked = True }
        _ -> pu
