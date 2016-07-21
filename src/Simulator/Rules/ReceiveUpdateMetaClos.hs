module Simulator.Rules.ReceiveUpdateMetaClos where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.CallRecord as CallRecord

import Simulator.Rules.Base

receiveUpdateMetaClos :: Rule
receiveUpdateMetaClos acqua  =
  case (interconnection acqua) of
      ((ConstMsgUpdateMetaClos (MsgUpdateMetaClos pId pointer fnN count missing) 0):ms) -> trace ((show (PU.puId pu)) ++ ": receive updateMetaClos") $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg

          callRec' = callRec { functionName = fnN, CallRecord.paramCount = count, CallRecord.paramMissing = missing }

          crseg' = Map.insert (addr pointer) (CallRecordV callRec') crseg
          pu' = pu { callRecordSeg = crseg', locked = True }

          pus' = updatePU pus pu'
      _ -> acqua
