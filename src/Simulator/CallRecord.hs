module Simulator.CallRecord where

import Data.Sequence

import AcquaIR.Language
import Simulator.Value

data CallRecord = CallRecord {
  functionName :: Name,
  paramMissing :: Int,
  paramCount :: Int,
  params :: Seq Value
} deriving (Show,Eq)

emptyCallRecord :: CallRecord
emptyCallRecord = CallRecord "" 0 0 (fromList [])

callRecordWithIntParam :: Int -> CallRecord
callRecordWithIntParam param = CallRecord "" 0 0 (fromList [(NumberV param)])

callRecordWithListParam :: Pointer -> CallRecord
callRecordWithListParam param = CallRecord "" 0 0 (fromList [(PointerV param)])
