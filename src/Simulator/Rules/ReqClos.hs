module Simulator.Rules.ReqClos where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value
import Simulator.Heap
import Simulator.CallRecord as CallRecord


import Simulator.Rules.Base

reqClos :: Rule
reqClos acqua  =
  case (interconnection acqua) of
      ((ConstMsgReqClos (MsgReqClos pIdS ptSrc pIdT ptTrg)):ms) -> trace ((show (PU.puId pu)) ++ ": receive ReqClos")  $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus
          omq' = (outgoingMessageQueue pu) ++ newMessages
          newMessages = updMetaMsg ++ updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS))
          updMsgs = toList $ Seq.mapWithIndex idxValToMsg (params callRec)
          updMetaMsg = [ConstMsgUpdateMetaClos (MsgUpdateMetaClos pIdS ptSrc (functionName callRec) (CallRecord.paramCount callRec) (CallRecord.paramMissing callRec))]
          Just (CallRecordV callRec) = Map.lookup (addr ptTrg) (heap pu)
          idxValToMsg idx val =
              ConstMsgUpdateClos (MsgUpdateClos pIdS ptSrc idx val)

          pu' = pu { outgoingMessageQueue = omq', locked = True }
          pus' = updatePU pus pu'

      _ -> acqua
