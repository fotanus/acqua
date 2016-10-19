module Simulator.Rules.CallRecord.SetCallRecordParamIL where

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

setCallRecordParamIL :: Rule
setCallRecordParamIL acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetCallRecordParamIL (processingUnits acqua)
    stepSetCallRecordParamIL pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetCallRecordParamIL x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetCallRecordParamIL " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (CallRecordV callRecord) = Map.lookup (addr pointer) crseg
            params' = Sequence.update i (LabelV v) (params callRecord)
            callRecord' = callRecord { params = params' }
            crseg' = Map.insert (addr pointer) (CallRecordV callRecord') crseg
            pu' = pu { PU.commands = cs, callRecordSeg = crseg', locked = True }
        _ -> pu
