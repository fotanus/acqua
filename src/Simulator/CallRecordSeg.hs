module Simulator.CallRecordSeg where

import Data.Map as Map

import Simulator.Value
import Simulator.CallRecord

type CallRecordSeg = Map CallRecordSegAddr CallRecordSegValue

data CallRecordSegValue
  = CallRecordV CallRecord
  deriving (Show,Eq)

nextFreePos :: CallRecordSeg -> CallRecordSegAddr
nextFreePos callRecordSeg = Map.size callRecordSeg

lookup :: CallRecordSegAddr -> CallRecordSeg -> CallRecordSegValue
lookup ad crseg =
  let
    Just v = Map.lookup ad crseg
  in
    v

lookupPt :: Pointer -> CallRecordSeg -> CallRecordSegValue
lookupPt pt crseg =
  let
    Just v = Map.lookup (addr pt) crseg
  in
    v
