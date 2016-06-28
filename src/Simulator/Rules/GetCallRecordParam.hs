module Simulator.Rules.GetCallRecordParam where

import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

getCallRecordParam :: Rule
getCallRecordParam acqua =
    acqua { processingUnits = newPus }
  where
    newPus = map stepGetCallRecordParam (processingUnits acqua)
    stepGetCallRecordParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetCallRecordParam x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetCallRecordParam " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            PointerV pointer = getVal pu x
            CallRecordV callRecord = Heap.lookupPt pointer (heap pu)
            val = Sequence.index (params callRecord) i
            pu' = (setVal pu v val) { PU.commands = cs, locked = True }
        _ -> pu