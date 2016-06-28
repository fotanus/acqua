module Simulator.Rules.NewCallRecord where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.Heap as Heap
import Simulator.CallRecord

import Simulator.Rules.Base

newCallRecord :: Rule
newCallRecord acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepNewCallRecord (processingUnits acqua)
    stepNewCallRecord pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((NewCallRecord x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": NewCallRecord " ++ (show x) ++ " " ++ (show n)) pu'
          where
            hp = heap pu
            hpPos = Heap.nextFreePos hp
            pointer = Pointer (PU.puId pu) hpPos
            clos = CallRecord "" 0 0 (Sequence.replicate n (NumberV 0))
            hp' = Map.insert hpPos (CallRecordV clos) hp
            pu' = (setVal pu x (PointerV pointer)) { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
