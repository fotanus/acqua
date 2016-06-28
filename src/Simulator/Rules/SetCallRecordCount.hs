module Simulator.Rules.SetCallRecordCount where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg as CallRecordSeg
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
            crseg = callRecordSeg pu
            PointerV pointer = getVal pu x
            NumberV num = getVal pu n
            CallRecordV callRec = CallRecordSeg.lookupPt pointer crseg
            callRec' = callRec { paramCount = num }
            crseg' = Map.insert (addr pointer) (CallRecordV callRec') crseg
            pu' = pu { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
