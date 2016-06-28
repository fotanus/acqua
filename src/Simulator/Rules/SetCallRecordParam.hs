module Simulator.Rules.SetCallRecordParam where

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

setCallRecordParam :: Rule
setCallRecordParam acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordParam (processingUnits acqua)
    stepSetCallRecordParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordParam x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordParam " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRecord) = Map.lookup (addr pointer) hp
            Just (NumberV idx) = Map.lookup i cenv
            Just val = Map.lookup v cenv
            params' = Sequence.update idx val (params callRecord)
            callRecord' = callRecord { params = params' }
            hp' = Map.insert (addr pointer) (CallRecordV callRecord') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
