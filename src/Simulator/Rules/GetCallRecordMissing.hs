module Simulator.Rules.GetCallRecordMissing where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

getCallRecordMissing :: Rule
getCallRecordMissing acqua  =
    acqua { processingUnits = newPus }
  where
    newPus = map stepGetCallRecordMissing (processingUnits acqua)
    stepGetCallRecordMissing pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetCallRecordMissing x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetCallRecordMissing " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PointerV pointer = getVal pu x
            CallRecordV callRecord = Heap.lookupPt pointer (heap pu)
            missing = (paramMissing callRecord)
            pu' = (setVal pu n (NumberV missing)) { PU.commands = cs, locked = True }
        _ -> pu
