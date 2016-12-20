module Simulator.Rules.List.Slice where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

sliceRule :: Rule
sliceRule acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Slice x l1 x1 x2):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = Slice " ++ (show l1) ++ " " ++ (show x1) ++ " " ++ (show x2)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer1) = Map.lookup l1 cenv
            Just (NumberV start) = Map.lookup x1 cenv
            Just (NumberV end) = Map.lookup x2 cenv
            Just (ListV list1) = Map.lookup (addr pointer1) crseg

            newListVals = take (end - start + 1) (drop start (params list1))

            crsegPos = nextFreePos crseg
            newPointer = Pointer (PU.puId pu) crsegPos
            newList = List (length newListVals) newListVals
            crseg' = Map.insert crsegPos (ListV newList) crseg
            pu' = (setVal pu x (PointerV newPointer)) { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
