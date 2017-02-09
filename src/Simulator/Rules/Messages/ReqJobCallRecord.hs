module Simulator.Rules.Messages.ReqJobCallRecord where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value
import Simulator.CallRecordSeg
import Simulator.CallRecord as CR


import Simulator.Rules.Base

getNextReqJobCallRecordMessage :: Interconnection -> Maybe Message
getNextReqJobCallRecordMessage [] = Nothing
getNextReqJobCallRecordMessage (m:ms) =
   case m of
       ConstMsgReqJobCallRecord  _ 0 -> Just m
       _ -> getNextReqJobCallRecordMessage ms

reqJobCallRecord :: Rule
reqJobCallRecord acqua  =
  let m = getNextReqJobCallRecordMessage (interconnection acqua)
  in case m of
      Just (ConstMsgReqJobCallRecord (MsgReqJobCallRecord pIdS pointerS pIdT pointerT) 0) -> trace (("[message "++(show (PU.puId pu))++"]") ++ ": receive ReqJobCallRecord for" ++ (show pointerS))  $ reqJobCallRecord $ acqua { processingUnits = pus', interconnection = iret }
        where
          Just m' = m
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus

          omq = outgoingMessageQueue pu
          crseg = callRecordSeg pu
          omq' = omq ++ newMessages
          newMessages = [updMetaMsg] ++ updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndReqCopy (MsgEndReqCopy pIdS) (msgStepsToPropagate acqua) )
          updMsgs = toList $ if (isMap callRec)
                             then let
                                    mapparams = (params callRec)
                                    nParams = Seq.length mapparams
                                   in
                                    Seq.mapWithIndex idxValToMsg (Seq.take (nParams-1) mapparams)
                             else Seq.mapWithIndex idxValToMsg (params callRec)

          updMetaMsg = ConstMsgUpdateMetaPointer (MsgUpdateMetaPointer pIdS pointerS (functionName callRec) (CR.paramCount callRec) (CR.paramMissing callRec) (isMap callRec)) (msgStepsToPropagate acqua)
          Just (CallRecordV callRec) = Map.lookup (addr pointerT) crseg

          crseg' = if not $ isMap callRec
                   then trace ("Deleting " ++ (show pointerT)) (Map.delete (addr pointerT) crseg)
                   else crseg

          idxValToMsg idx val =
              ConstMsgUpdate (MsgUpdate pIdS pointerS idx val) (msgStepsToPropagate acqua)

          pu' = pu { outgoingMessageQueue = omq', locked = True, callRecordSeg = crseg' }
          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgReqJobCallRecord (MsgReqJobCallRecord pIdS pointerS pIdT pointerT) 1)]
          (iret, pus') = if lockedMsg pu
                         then (i'', pus)
                         else (i', updatePU pus pu')

      _ -> acqua
