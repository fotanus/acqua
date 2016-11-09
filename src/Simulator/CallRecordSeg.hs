module Simulator.CallRecordSeg where

import Debug.Trace
import Data.Map as Map

import Simulator.Value
import Simulator.CallRecord
import Simulator.List

type CallRecordSeg = Map CallRecordSegAddr CallRecordSegValue

data CallRecordSegValue
  = CallRecordV CallRecord
  | ListV List
  deriving (Show,Eq)

nextFreePos :: CallRecordSeg -> CallRecordSegAddr
nextFreePos callRecordSeg = nextPos 0
  where
    nextPos n = case Map.lookup n callRecordSeg of
                Nothing -> n
                _ -> nextPos (n+1)

lookup :: CallRecordSegAddr -> CallRecordSeg -> CallRecordSegValue
lookup ad crseg =
  let
    Just v = Map.lookup ad crseg
  in
    v

lookupPt :: Pointer -> CallRecordSeg -> CallRecordSegValue
lookupPt pt crseg =
  let
    Just v = traceShowId $ Map.lookup (addr pt) crseg
  in
    v
