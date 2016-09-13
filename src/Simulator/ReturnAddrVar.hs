module Simulator.ReturnAddrVar where

import AcquaIR.Language
import Simulator.Value

data ReturnAddrVar
  = EnvVal Name
  | ListVal Pointer Int
  deriving (Show,Eq)

