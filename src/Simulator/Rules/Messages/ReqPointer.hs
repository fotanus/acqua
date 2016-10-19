module Simulator.Rules.Messages.ReqPointer where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value
import Simulator.List
import Simulator.CallRecordSeg
import Simulator.CallRecord as CR 


import Simulator.Rules.Base

getNextReqPointerMessage :: Interconnection -> Maybe Message
getNextReqPointerMessage [] = Nothing
getNextReqPointerMessage (m:ms) =
   case m of
       ConstMsgReqPointer  _ 0 -> Just m
       _ -> getNextReqPointerMessage ms


reqPointer :: Rule
reqPointer acqua  =
  let m = getNextReqPointerMessage (interconnection acqua)
  in case m of
      Just (ConstMsgReqPointer (MsgReqPointer pIdS ptSrc pIdT ptTrg) 0) -> trace ((show (PU.puId pu)) ++ ": receive ReqPointer")  $ reqPointer $ acqua { processingUnits = pus', interconnection = iret }
        where
          Just m' = m
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus
          omq' = (outgoingMessageQueue pu) ++ newMessages

          newMessages = case Map.lookup (addr ptTrg) (callRecordSeg pu) of
                        Nothing -> error $ "reqclos: asked for unexisting call record or list"
                        Just (ListV (List listSize listItems)) ->
                          let
                             endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS) (msgStepsToPropagate acqua))
                             updMsgs = map (\val -> ConstMsgUpdateList (MsgUpdateList pIdS ptSrc val) (msgStepsToPropagate acqua)) listItems
                             updMetaMsg = ConstMsgUpdateMetaList (MsgUpdateMetaList pIdS ptSrc listSize) (msgStepsToPropagate acqua)
                          in
                             [updMetaMsg] ++ updMsgs ++ [endMsg]
                        Just (CallRecordV callRec) ->
                          let
                             endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS) (msgStepsToPropagate acqua))
                             updMsgs = toList $ Seq.mapWithIndex idxValToMsg (CR.params callRec)
                             updMetaMsg = ConstMsgUpdateMetaPointer (MsgUpdateMetaPointer pIdS ptSrc (functionName callRec) (CR.paramCount callRec) (CR.paramMissing callRec) (isMap callRec)) (msgStepsToPropagate acqua)
                             idxValToMsg idx val =
                                 ConstMsgUpdatePointer (MsgUpdatePointer pIdS ptSrc idx val) (msgStepsToPropagate acqua)
                          in
                             [updMetaMsg] ++ updMsgs ++ [endMsg]

          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgReqPointer (MsgReqPointer pIdS ptSrc pIdT ptTrg) 1)]
          pu' = pu { outgoingMessageQueue = omq', locked = True }
          (iret, pus') = if lockedMsg pu
                         then (i'', pus)
                         else (i', updatePU pus pu')

      _ -> acqua
