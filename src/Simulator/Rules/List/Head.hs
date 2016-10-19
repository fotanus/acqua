module Simulator.Rules.List.Head where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

headRule :: Rule
headRule acqua = acqua { processingUnits = newPus }
  where
    newPus = map steplistParam (processingUnits acqua)
    steplistParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Head x l):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = Head " ++ (show l)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup l cenv
            Just (ListV list) = Map.lookup (addr pointer) crseg
            val = case (params list) of
                  []     -> traceShow pu $ error $ "error on headRule: head on empty list. "
                  (p:_) -> p
            pu' = (setVal pu x val) { PU.commands = cs, locked = True }
        _ -> pu
