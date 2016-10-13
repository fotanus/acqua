module Simulator.Rules.StepMessage where

import Simulator.Acqua
import Simulator.Interconnection as PU
import Simulator.Rules.Base

stepMessage :: Rule
stepMessage acqua = acqua { interconnection = i' }
  where
    i' = map decMsgStepCount (interconnection acqua)
    decMsgStepCount msg = case msg of
           ConstMsgUpdate m c -> if c > 0 then ConstMsgUpdate m (c-1) else ConstMsgUpdate m c
           ConstMsgUpdateClos m c -> if c > 0 then ConstMsgUpdateClos m (c-1) else ConstMsgUpdateClos m c
           ConstMsgUpdateMetaClos m c -> if c > 0 then ConstMsgUpdateMetaClos m (c-1) else ConstMsgUpdateMetaClos m c
           ConstMsgUpdateList m c -> if c > 0 then ConstMsgUpdateList m (c-1) else ConstMsgUpdateList m c
           ConstMsgUpdateMetaList m c -> if c > 0 then ConstMsgUpdateMetaList m (c-1) else ConstMsgUpdateMetaList m c
           ConstMsgResponse m c -> if c > 0 then ConstMsgResponse m (c-1) else ConstMsgResponse m c
           ConstMsgReqEnv m c -> if c > 0 then ConstMsgReqEnv m (c-1) else ConstMsgReqEnv m c
           ConstMsgReqClos m c -> if c > 0 then ConstMsgReqClos m (c-1) else ConstMsgReqClos m c
           ConstMsgEndCopy m c -> if c > 0 then ConstMsgEndCopy m (c-1) else ConstMsgEndCopy m c
           ConstMsgEndReqCopy m c -> if c > 0 then ConstMsgEndReqCopy m (c-1) else ConstMsgEndReqCopy m c

