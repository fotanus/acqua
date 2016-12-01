module Simulator.Value where

import AcquaIR.Language
import Simulator.ProcessingUnitId

type CallRecordSegAddr = Int

data Pointer = Pointer {
  puId :: PId,
  addr :: CallRecordSegAddr
} deriving (Show, Eq)

data Value
  = LabelV Label
  | NumberV Int
  | PointerV Pointer
  | NameV Name
  deriving (Show,Eq)

numVal :: Value -> Int
numVal (NumberV n) = n
numVal _ = error "numVal must receive a NumberV"
