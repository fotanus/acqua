module Simulator.Rules.GetCallRecordCount where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg as CallRecordSeg
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

getCallRecordCount :: Rule
getCallRecordCount acqua =
    acqua { processingUnits = map stepGetCallRecordCount (processingUnits acqua) }
  where
    stepGetCallRecordCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetCallRecordCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetCallRecordCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PointerV pointer = getVal pu x
            CallRecordV callRecord = CallRecordSeg.lookupPt pointer (callRecordSeg pu)
            count = (paramCount callRecord)
            pu' = (setVal pu n (NumberV count)) { PU.commands = cs, locked = True }
        _ -> pu
