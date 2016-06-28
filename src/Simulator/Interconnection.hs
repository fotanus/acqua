module Simulator.Interconnection where

import AcquaIR.Language
import Simulator.Value
import Simulator.ProcessingUnitId

data Message
 = ConstMsgUpdate  MsgUpdate
 | ConstMsgUpdateClos  MsgUpdateClos
 | ConstMsgUpdateMetaClos  MsgUpdateMetaClos
 | ConstMsgResponse MsgResponse
 | ConstMsgReqEnv  MsgReqEnv
 | ConstMsgReqClos MsgReqClos
 | ConstMsgEndCopy MsgEndCopy
 deriving (Show, Eq)


data MsgUpdate = MsgUpdate {
  puId :: PId,
  environment :: EnvId,
  index :: Int,
  value :: Value
  } deriving (Show, Eq)

data MsgUpdateClos = MsgUpdateClos {
  puIdC :: PId,
  ptC :: Pointer,
  indexC :: Int,
  valueC :: Value
  } deriving (Show, Eq)

data MsgUpdateMetaClos = MsgUpdateMetaClos {
  puIdMC :: PId,
  ptM :: Pointer,
  fnName :: String,
  paramCount :: Int,
  paramMissing :: Int
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
  callRecord :: Name
} deriving (Show, Eq)

data MsgReqClos = MsgReqClos {
  puIdSC :: PId,
  ptS :: Pointer,
  puIdTC :: PId,
  ptT :: Pointer
} deriving (Show, Eq)


data MsgEndCopy = MsgEndCopy {
  puIdEC :: PId
} deriving (Show, Eq)


type Interconnection = [Message]

newInterconnection :: Interconnection
newInterconnection = []
