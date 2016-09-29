module Simulator.Rules.Length where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

lengthRule :: Rule
lengthRule acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Length x l):cs),True) -> trace ((show (PU.puId pu)) ++ ": " ++ (show x) ++ " = Length " ++ (show l)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup l cenv
            Just (ListV list) = Map.lookup (addr pointer) crseg
            val = length (params list)
            pu' = (setVal pu x (NumberV val)) { PU.commands = cs, locked = True }
        _ -> pu
