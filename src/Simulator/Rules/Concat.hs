module Simulator.Rules.Concat where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

concatRule :: Rule
concatRule acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Concat x l1 l2):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = Concat " ++ (show l1) ++ " " ++ (show l2)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer1) = Map.lookup l1 cenv
            Just (PointerV pointer2) = Map.lookup l2 cenv
            Just (ListV list1) = Map.lookup (addr pointer1) crseg
            Just (ListV list2) = Map.lookup (addr pointer2) crseg

            newListVals = (params list1) ++ (params list2)

            crsegPos = nextFreePos crseg
            newPointer = Pointer (PU.puId pu) crsegPos
            newList = List (length newListVals) newListVals
            crseg' = Map.insert crsegPos (ListV newList) crseg
            pu' = (setVal pu x (PointerV newPointer)) { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
