module Simulator.Job where

import AcquaIR.Language
import Simulator.ProcessingUnitId
import Simulator.ReturnAddrVar
import Simulator.Value

data CopySource
  = CallSource Pointer 
  | MapSource Pointer Value
  | SMapSource Pointer Pointer Int
  deriving (Show,Eq)

data Job = Job {
  puId :: PId,
  copySource :: CopySource,
  copySourceSize :: Int,
  environment :: EnvId,
  variable :: ReturnAddrVar,
  isMap :: Bool
} deriving(Show, Eq)

