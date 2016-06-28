module Simulator.Rules.SetCallRecordFn where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordFn :: Rule
setCallRecordFn acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordFn (processingUnits acqua)
    stepSetCallRecordFn pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordFn x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordFn " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRec) = Map.lookup (addr pointer) hp

            callRec' = callRec { functionName = n }
            hp' = Map.insert (addr pointer) (CallRecordV callRec') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
