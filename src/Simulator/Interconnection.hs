module Simulator.Interconnection where

import AcquaIR.Language
import Simulator.Environment

type Interconnection = [Message]
type PId = Int

data Message
 = ConstMsgUpdate  MsgUpdate
 | ConstMsgResponse MsgResponse
 | ConstMsgReqEnv  MsgReqEnv
 | ConstMsgEndCopy MsgEndCopy
 deriving (Show, Eq)

data MsgUpdate = MsgUpdate {
  puId :: PId,
  environment :: EnvId,
  index :: Int,
  value :: BaseVal
  } deriving (Show, Eq)

data MsgResponse = MsgResponse {
  puIdR :: PId,
  environmentR :: EnvId,
  variableR :: Name,
  valueR :: Value
  } deriving (Show, Eq)

data MsgReqEnv = MsgReqEnv {
  puIdS :: PId,
  jsId  :: EnvId,
  puIdT :: PId,
  teId  :: EnvId,
  closure :: Name
} deriving (Show, Eq)


data MsgEndCopy = MsgEndCopy {
  puIdEC :: PId
} deriving (Show, Eq)


newInterconnection :: Interconnection
newInterconnection = []
