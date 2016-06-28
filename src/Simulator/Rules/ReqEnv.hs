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
import Simulator.Heap
import Simulator.CallRecord


import Simulator.Rules.Base

reqEnv :: Rule
reqEnv acqua  =
  case (interconnection acqua) of
      ((ConstMsgReqEnv (MsgReqEnv pIdS mjsId pIdT mteId callRecordName)):ms) -> trace ((show (PU.puId pu)) ++ ": receive ReqEnv")  $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus

          omq = outgoingMessageQueue pu
          env = environments pu
          hp = heap pu
          omq' = omq ++ newMessages
          newMessages = updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS))
          updMsgs = toList $ Seq.mapWithIndex idxValToMsg (params callRec)
          Just tenv = Map.lookup mteId env
          Just (PointerV pointer) = Map.lookup callRecordName tenv
          Just (CallRecordV callRec) = Map.lookup (addr pointer) hp
          idxValToMsg idx val =
              ConstMsgUpdate (MsgUpdate pIdS mjsId idx val)

          pu' = pu { outgoingMessageQueue = omq', locked = True }
          pus' = updatePU pus pu'

      _ -> acqua
