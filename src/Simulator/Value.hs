module Simulator.Value where

import AcquaIR.Language
import Simulator.ProcessingUnitId

type HeapAddr = Int

data Pointer = Pointer {
  puId :: PId,
  addr :: HeapAddr
} deriving (Show, Eq)

data Value
  = LabelV Label
  | NumberV Int
  | PointerV Pointer
  deriving (Show,Eq)
