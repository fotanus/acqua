module Simulator.Interconnection where

import AcquaIR.Language
import Simulator.ProcessingUnit
import Simulator.Environment


type Interconnection = [Message]

data Message
 = ConstMsgUpdate  MsgUpdate
 | ConstMsgReqEnv  MsgReqEnv
 | ConstMsgEndCopy MsgEndCopy
 deriving (Show, Eq)

data MsgUpdate = MsgUpdate {
  puId :: PId,
  environment :: EnvId,
  variable :: Name,
  value :: Value
  } deriving (Show, Eq)

data MsgReqEnv = MsgReqEnv {
  puIdS :: PId,
  jsId  :: EnvId,
  puIdT :: PId,
  teId  :: EnvId
} deriving (Show, Eq)


data MsgEndCopy = MsgEndCopy {
  puIdEC :: PId
} deriving (Show, Eq)


newInterconnection :: Interconnection
newInterconnection = []
