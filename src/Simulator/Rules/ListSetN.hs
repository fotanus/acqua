module Simulator.Rules.ListSetN where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

listSetN :: Rule
listSetN acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((ListSetN x i identifier):cs),True) -> trace ((show (PU.puId pu)) ++ ": ListSetN " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (NumberV v) = Map.lookup identifier cenv
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ListV list) = Map.lookup (addr pointer) crseg
            params' = listSetPos (params list) i (NumberV v)
            list' = list { params = params' }
            crseg' = Map.insert (addr pointer) (ListV list') crseg
            pu' = pu { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
