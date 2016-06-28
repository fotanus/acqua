module Simulator.Rules.SetCallRecordMissingI where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordMissingI :: Rule
setCallRecordMissingI acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordMissingI (processingUnits acqua)
    stepSetCallRecordMissingI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordMissingI x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordMissingI " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRecord) = Map.lookup (addr pointer) hp
            callRecord' = callRecord { paramMissing = n }
            hp' = Map.insert (addr pointer) (CallRecordV callRecord') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
