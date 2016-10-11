module Simulator.CallRecord where

import Data.Sequence

import AcquaIR.Language
import Simulator.Value


data CallRecord = CallRecord {
  functionName :: Name,
  paramMissing :: Int,
  paramCount :: Int,
  params :: Seq Value,
  isMap :: Bool,
  timeout :: Int
} deriving (Show,Eq)

maxTimeout :: Int
maxTimeout = 1000

decreaseTimeout :: CallRecord -> CallRecord
decreaseTimeout cr = if isMap cr && timeout cr <= maxTimeout
                     then cr { timeout = (timeout cr) - 1 }
                     else cr

emptyCallRecord :: CallRecord
emptyCallRecord = CallRecord "" 0 0 (fromList []) False (maxTimeout + 1)

callRecordWithIntParam :: Int -> CallRecord
callRecordWithIntParam param = CallRecord "" 0 0 (fromList [(NumberV param)]) False (maxTimeout + 1)

callRecordWithListParam :: Pointer -> CallRecord
callRecordWithListParam param = CallRecord "" 0 0 (fromList [(PointerV param)]) False (maxTimeout + 1)
