module Simulator.Environment where

import Data.Map as Map

import AcquaIR.Language
import Simulator.Value

type Environment = Map Name Value

emptyEnv :: Environment
emptyEnv = Map.fromList []

