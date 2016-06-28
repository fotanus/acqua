module Simulator.Rules.SetCallRecordCount where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordCount :: Rule
setCallRecordCount acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordCount (processingUnits acqua)
    stepSetCallRecordCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            hp = heap pu
            PointerV pointer = getVal pu x
            NumberV num = getVal pu n
            CallRecordV callRec = Heap.lookupPt pointer hp
            callRec' = callRec { paramCount = num }
            hp' = Map.insert (addr pointer) (CallRecordV callRec') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
