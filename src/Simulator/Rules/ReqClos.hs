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
import Simulator.Closure as Closure


import Simulator.Rules.Base

reqClos :: Rule
reqClos acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      ((ConstMsgReqClos (MsgReqClos pIdS ptS pIdT ptT)):ms) -> trace ((show (PU.puId pu)) ++ ": receive ReqClos")  $ Acqua bb q pus' ms f s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pIdT) pus
          omq' = (outgoingMessageQueue pu) ++ newMessages
          newMessages = updMetaMsg ++ updMsgs ++ [endMsg]
          endMsg = (ConstMsgEndCopy (MsgEndCopy pIdS))
          updMsgs = toList $ Seq.mapWithIndex idxValToMsg (params closur)
          updMetaMsg = [ConstMsgUpdateMetaClos (MsgUpdateMetaClos pIdS ptS (functionName closur) (Closure.paramCount closur) (Closure.paramMissing closur))]
          Just (ClosureV closur) = Map.lookup (addr ptT) (heap pu)
          idxValToMsg idx val =
              ConstMsgUpdateClos (MsgUpdateClos pIdS ptS idx val)

          pu' = pu { outgoingMessageQueue = omq', locked = True }
          pus' = updatePU pus pu'

      _ -> acqua
