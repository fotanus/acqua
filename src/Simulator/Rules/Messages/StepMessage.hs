module Simulator.Rules.Messages.StepMessage where

import Simulator.Acqua
import Simulator.Interconnection as PU
import Simulator.Rules.Base

stepMessage :: Rule
stepMessage acqua = acqua { interconnection = i' }
  where
    i' = map decMsgStepCount (interconnection acqua)
    decMsgStepCount msg = case msg of
           ConstMsgUpdate m c -> if c > 0 then ConstMsgUpdate m (c-1) else ConstMsgUpdate m c
           ConstMsgUpdatePointer m c -> if c > 0 then ConstMsgUpdatePointer m (c-1) else ConstMsgUpdatePointer m c
           ConstMsgUpdateMetaPointer m c -> if c > 0 then ConstMsgUpdateMetaPointer m (c-1) else ConstMsgUpdateMetaPointer m c
           ConstMsgUpdateList m c -> if c > 0 then ConstMsgUpdateList m (c-1) else ConstMsgUpdateList m c
           ConstMsgUpdateMetaList m c -> if c > 0 then ConstMsgUpdateMetaList m (c-1) else ConstMsgUpdateMetaList m c
           ConstMsgResponse m c -> if c > 0 then ConstMsgResponse m (c-1) else ConstMsgResponse m c
           ConstMsgReqJobCallRecord m c -> if c > 0 then ConstMsgReqJobCallRecord m (c-1) else ConstMsgReqJobCallRecord m c
           ConstMsgReqPointer m c -> if c > 0 then ConstMsgReqPointer m (c-1) else ConstMsgReqPointer m c
           ConstMsgEndCopy m c -> if c > 0 then ConstMsgEndCopy m (c-1) else ConstMsgEndCopy m c
           ConstMsgEndReqCopy m c -> if c > 0 then ConstMsgEndReqCopy m (c-1) else ConstMsgEndReqCopy m c

