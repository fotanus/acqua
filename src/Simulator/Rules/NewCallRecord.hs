module Simulator.Rules.NewCallRecord where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.CallRecordSeg as CallRecordSeg
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
            crseg = callRecordSeg pu
            crsegPos = CallRecordSeg.nextFreePos crseg
            pointer = Pointer (PU.puId pu) crsegPos
            clos = CallRecord "" 0 0 (Sequence.replicate n (NumberV 0))
            crseg' = Map.insert crsegPos (CallRecordV clos) crseg
            pu' = (setVal pu x (PointerV pointer)) { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
