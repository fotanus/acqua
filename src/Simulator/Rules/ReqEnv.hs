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

reqEnv :: Rule
reqEnv acqua  =
  case (interconnection acqua) of
      ((ConstMsgReqEnv (MsgReqEnv pIdS mjsId pIdT mteId callRecordName) 0):ms) -> trace ((show (PU.puId pu)) ++ ": receive ReqEnv for" ++ callRecordName)  $ acqua { processingUnits = pus', interconnection = ms }
        where
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
          -- TODO: deallocate here, count number of calls on cr because of map
          crseg' = crseg -- Map.delete (addr pointer) crseg
          idxValToMsg idx val =
              ConstMsgUpdate (MsgUpdate pIdS mjsId idx val) (msgStepsToPropagate acqua)

          pu' = pu { outgoingMessageQueue = omq', locked = True, callRecordSeg = crseg' }
          pus' = updatePU pus pu'

      _ -> acqua
