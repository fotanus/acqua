module Simulator.Rules.SetCallRecordCountI where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

setCallRecordCountI :: Rule
setCallRecordCountI acqua =
    acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordCountI (processingUnits acqua)
    stepSetCallRecordCountI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordCountI x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordCountI " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRecord) = Map.lookup (addr pointer) crseg
            callRecord' = callRecord { paramCount = n }
            crseg' = Map.insert (addr pointer) (CallRecordV callRecord') crseg
            pu' = pu { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
