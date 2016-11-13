module Simulator.Rules.CallRecord.SetCallRecordParam where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordParam :: Rule
setCallRecordParam acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordParam (processingUnits acqua)
    stepSetCallRecordParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordParam x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordParam " ++ (show x) ++ " " ++ (show idx) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRecord) = Map.lookup (addr pointer) crseg
            Just (NumberV idx) = Map.lookup i cenv
            Just val = Map.lookup v cenv
            params' = Sequence.update idx val (params callRecord)
            callRecord' = callRecord { params = params' }
            crseg' = Map.insert (addr pointer) (CallRecordV callRecord') crseg
            pu' = pu { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
