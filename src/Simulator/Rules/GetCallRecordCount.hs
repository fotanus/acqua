module Simulator.Rules.GetCallRecordCount where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

getCallRecordCount :: Rule
getCallRecordCount (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetCallRecordCount pus) i f s
  where
    stepGetCallRecordCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetCallRecordCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetCallRecordCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PointerV pointer = getVal pu x
            CallRecordV callRecord = Heap.lookupPt pointer (heap pu)
            count = (paramCount callRecord)
            pu' = (setVal pu n (NumberV count)) { PU.commands = cs, locked = True }
        _ -> pu
