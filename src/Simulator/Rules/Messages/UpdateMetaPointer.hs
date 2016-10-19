module Simulator.Rules.Messages.UpdateMetaPointer where

import Data.List
import qualified Data.Map as Map
import Data.Sequence as Sequence
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.CallRecord as CallRecord

import Simulator.Rules.Base

getNextUpdateMessage :: Interconnection -> Maybe Message
getNextUpdateMessage [] = Nothing
getNextUpdateMessage (m:ms) =
   case m of
       ConstMsgUpdateMetaPointer _ 0 -> Just m
       _ -> getNextUpdateMessage ms


updateMetaPointer :: Rule
updateMetaPointer acqua  =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgUpdateMetaPointer (MsgUpdateMetaPointer pId pointer fnN count missing im) 0) -> trace ((show (PU.puId pu)) ++ ": receive updateMetaPointer") $ updateMetaPointer $ acqua { processingUnits = pus', interconnection = iret}
        where
          Just m' = m
          i = interconnection acqua
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg
          callRec' = if (functionName callRec) == "assignCopyCR"
                     then emptyCallRecord { params = (Sequence.replicate (count+missing) (NumberV 0)) }
                     else callRec
          callRec'' = callRec' { functionName = fnN, CallRecord.paramCount = count, CallRecord.paramMissing = missing, isMap = im , timeout = if im then maxTimeout else maxTimeout + 1 }

          crseg' = Map.insert (addr pointer) (CallRecordV callRec'') crseg

          (iret, pu') = if (lockedMsg pu)
                        then (i'', pu)
                        else (i',  pu { callRecordSeg = crseg', lockedMsg = True })
          i' = delete m' i
          i'' = (ConstMsgUpdateMetaPointer (MsgUpdateMetaPointer pId pointer fnN count missing im) 1):i'

          pus' = updatePU pus pu'
      _ -> acqua
