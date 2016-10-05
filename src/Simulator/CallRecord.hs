module Simulator.CallRecord where

import Data.Sequence

import AcquaIR.Language
import Simulator.Value

data CallRecord = CallRecord {
  functionName :: Name,
  paramMissing :: Int,
  paramCount :: Int,
  params :: Seq Value,
  copiesToDelete :: Int
} deriving (Show,Eq)

emptyCallRecord :: CallRecord
emptyCallRecord = CallRecord "" 0 0 (fromList []) 1

callRecordWithIntParam :: Int -> CallRecord
callRecordWithIntParam param = CallRecord "" 0 0 (fromList [(NumberV param)]) 1

callRecordWithListParam :: Pointer -> CallRecord
callRecordWithListParam param = CallRecord "" 0 0 (fromList [(PointerV param)]) 1
