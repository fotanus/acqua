module Simulator.Rules.List.ListGet where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

listGet :: Rule
listGet acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((ListGet n1 n2 n3):cs),True) -> trace ((show (PU.puId pu)) ++ ": " ++ n1 ++ " = ListGet " ++ n2 ++ " " ++ n3)  pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup n2 cenv
            Just (ListV list) = Map.lookup (addr pointer) crseg
            Just (NumberV idx) = Map.lookup n3 cenv
            val  = (params list) !! idx
            pu' = (setVal pu n1 val) { PU.commands = cs, locked = True }
        _ -> pu
