module Simulator.Rules.SetCallRecordParamI where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordParamI :: Rule
setCallRecordParamI acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordParamI (processingUnits acqua)
    stepSetCallRecordParamI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordParamI x idx v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordParamI " ++ (show x) ++ " " ++ (show idx) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRecord) = Map.lookup (addr pointer) hp
            Just val = traceShow cenv $ Map.lookup v cenv
            params' = Sequence.update idx val (params callRecord)
            callRecord' = callRecord { params = params' }
            hp' = Map.insert (addr pointer) (CallRecordV callRecord') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
