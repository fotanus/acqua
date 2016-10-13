module Simulator.Rules.ReqEnv where

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

getNextReqEnvMessage :: Interconnection -> Maybe Message
getNextReqEnvMessage [] = Nothing
getNextReqEnvMessage (m:ms) =
   case m of
       ConstMsgReqEnv  _ 0 -> Just m
       _ -> getNextReqEnvMessage ms

reqEnv :: Rule
reqEnv acqua  =
  let m = getNextReqEnvMessage (interconnection acqua)
  in case m of
      Just (ConstMsgReqEnv (MsgReqEnv pIdS pointerS pIdT pointerT) 0) -> trace ((show (PU.puId pu)) ++ ": receive ReqEnv for" ++ (show pointerS))  $ reqEnv $ acqua { processingUnits = pus', interconnection = iret }
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

          updMetaMsg = ConstMsgUpdateMetaClos (MsgUpdateMetaClos pIdS pointerS (functionName callRec) (CR.paramCount callRec) (CR.paramMissing callRec) (isMap callRec)) (msgStepsToPropagate acqua)
          Just (CallRecordV callRec) = Map.lookup (addr pointerT) crseg

          crseg' = if not $ isMap callRec
                   then Map.delete (addr pointerT) crseg
                   else crseg

          idxValToMsg idx val =
              ConstMsgUpdate (MsgUpdate pIdS pointerS idx val) (msgStepsToPropagate acqua)

          pu' = pu { outgoingMessageQueue = omq', locked = True, callRecordSeg = crseg' }
          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgReqEnv (MsgReqEnv pIdS pointerS pIdT pointerT) 1)]
          (iret, pus') = if lockedMsg pu
                         then (i'', pus)
                         else (i', updatePU pus pu')

      _ -> acqua
