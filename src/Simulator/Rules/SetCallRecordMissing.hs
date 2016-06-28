module Simulator.Rules.SetCallRecordMissing where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordMissing :: Rule
setCallRecordMissing acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordMissing (processingUnits acqua)
    stepSetCallRecordMissing pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordMissing x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordMissing " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRec) = Map.lookup (addr pointer) hp
            Just (NumberV num) = Map.lookup n cenv
            callRec' = callRec { paramMissing = num }
            hp' = Map.insert (addr pointer) (CallRecordV callRec') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
