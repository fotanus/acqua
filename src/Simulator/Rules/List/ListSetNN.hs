module Simulator.Rules.List.ListSetNN where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

listSetNN :: Rule
listSetNN acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((ListSetNN x i identifier):cs),True) -> trace ((show (PU.puId pu)) ++ ": ListSetNN " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show val)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just val = Map.lookup identifier cenv
            Just (PointerV pointer) = Map.lookup x cenv
            Just (NumberV idx) = Map.lookup i cenv
            Just (ListV list) = Map.lookup (addr pointer) crseg
            params' = listSetPos (params list) idx val
            list' = list { params = params' }
            crseg' = Map.insert (addr pointer) (ListV list') crseg
            pu' = pu { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
