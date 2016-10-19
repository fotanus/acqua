module Simulator.Interconnection where

import AcquaIR.Language
import Simulator.Value
import Simulator.ProcessingUnitId
import Simulator.ReturnAddrVar

data Message
 = ConstMsgUpdate  MsgUpdate Int
 | ConstMsgUpdatePointer  MsgUpdatePointer Int
 | ConstMsgUpdateMetaPointer MsgUpdateMetaPointer Int
 | ConstMsgUpdateList  MsgUpdateList Int
 | ConstMsgUpdateMetaList  MsgUpdateMetaList Int
 | ConstMsgResponse MsgResponse Int
 | ConstMsgReqJobCallRecord  MsgReqJobCallRecord Int
 | ConstMsgReqPointer MsgReqPointer Int
 | ConstMsgEndCopy MsgEndCopy Int
 | ConstMsgEndReqCopy MsgEndReqCopy Int
 deriving (Show, Eq)


data MsgUpdate = MsgUpdate {
  puId :: PId,
  ptr :: Pointer,
  index :: Int,
  value :: Value
  } deriving (Show, Eq)

data MsgUpdatePointer = MsgUpdatePointer {
  puIdC :: PId,
  ptC :: Pointer,
  indexC :: Int,
  valueC :: Value
  } deriving (Show, Eq)

data MsgUpdateMetaPointer = MsgUpdateMetaPointer {
  puIdMC :: PId,
  ptM :: Pointer,
  fnName :: String,
  paramCount :: Int,
  paramMissing :: Int,
  isMapFlag :: Bool
  } deriving (Show, Eq)

data MsgUpdateList = MsgUpdateList {
  puIdCC :: PId,
  ptCC :: Pointer,
  valueCC :: Value
  } deriving (Show, Eq)

data MsgUpdateMetaList = MsgUpdateMetaList {
  puIdMCC :: PId,
  ptMC :: Pointer,
  valC :: Int
  } deriving (Show, Eq)


data MsgResponse = MsgResponse {
  puIdR :: PId,
  environmentR :: EnvId,
  variableR :: ReturnAddrVar,
  valueR :: Value
  } deriving (Show, Eq)

data MsgReqJobCallRecord = MsgReqJobCallRecord {
  puIdS :: PId,
  ptrS :: Pointer,
  puIdT :: PId,
  ptrT :: Pointer
} deriving (Show, Eq)

data MsgReqPointer = MsgReqPointer {
  puIdSC :: PId,
  ptS :: Pointer,
  puIdTC :: PId,
  ptT :: Pointer
} deriving (Show, Eq)


data MsgEndCopy = MsgEndCopy {
  puIdEC :: PId
} deriving (Show, Eq)

data MsgEndReqCopy = MsgEndReqCopy {
  puIdERC :: PId
} deriving (Show, Eq)


type Interconnection = [Message]

newInterconnection :: Interconnection
newInterconnection = []
