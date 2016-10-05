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
import Simulator.CallRecord


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
      Just (ConstMsgReqEnv (MsgReqEnv pIdS mjsId pIdT mteId callRecordName) 0) -> trace ((show (PU.puId pu)) ++ ": receive ReqEnv for" ++ callRecordName)  $ reqEnv $ acqua { processingUnits = pus', interconnection = iret }
        where
          Just m' = m
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus

          omq = outgoingMessageQueue pu
          env = environments pu
          crseg = callRecordSeg pu
          omq' = omq ++ newMessages
          newMessages = updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS) (msgStepsToPropagate acqua) )
          updMsgs = toList $ Seq.mapWithIndex idxValToMsg (params callRec)
          Just tenv = Map.lookup mteId env
          Just (PointerV pointer) = Map.lookup callRecordName tenv
          Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg

          -- dealocate call record if count got to 0
          ctd = copiesToDelete callRec
          crseg' = if ctd == 1
                   then Map.delete (addr pointer) crseg
                   else Map.insert (addr pointer) (CallRecordV (callRec { copiesToDelete = ctd-1 })) crseg

          idxValToMsg idx val =
              ConstMsgUpdate (MsgUpdate pIdS mjsId idx val) (msgStepsToPropagate acqua)

          pu' = pu { outgoingMessageQueue = omq', locked = True, callRecordSeg = crseg' }
          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgReqEnv (MsgReqEnv pIdS mjsId pIdT mteId callRecordName) 1)]
          (iret, pus') = if lockedMsg pu
                         then (i'', pus)
                         else (i', updatePU pus pu')

      _ -> acqua
