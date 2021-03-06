module Simulator.Rules.List.NewListN where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.CallRecordSeg as CallRecordSeg
import Simulator.List

import Simulator.Rules.Base

newListN :: Rule
newListN acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepNewListN (processingUnits acqua)
    stepNewListN pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((NewListN x n1):cs),True) -> trace ((show (PU.puId pu)) ++ ": NewListN " ++ (show x) ++ " " ++ (show n)) pu'
          where
            crseg = callRecordSeg pu
            crsegPos = CallRecordSeg.nextFreePos crseg
            pointer = Pointer (PU.puId pu) crsegPos
            list = List n (take n (repeat (NumberV 0)))
            NumberV n = getVal pu n1
            crseg' = Map.insert crsegPos (ListV list) crseg
            pu' = (setVal pu x (PointerV pointer)) { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
