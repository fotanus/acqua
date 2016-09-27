module Simulator.Rules.Tail where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

tailRule :: Rule
tailRule acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Tail x l):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = Tail " ++ (show l)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup l cenv
            Just (ListV list) = Map.lookup (addr pointer) crseg
            newListVals = tail (params list)

            crsegPos = nextFreePos crseg
            newPointer = Pointer (PU.puId pu) crsegPos
            newList = List (length newListVals) newListVals
            crseg' = Map.insert crsegPos (ListV newList) crseg
            pu' = (setVal pu x (PointerV newPointer)) { PU.commands = cs, callRecordSeg = crseg', locked = True, stallCycles = (length newListVals) }
        _ -> pu
