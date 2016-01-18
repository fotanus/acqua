module Simulator.Environment where

import Data.Map

import AcquaIR.Language

data Value
 = LabelValue Label
 | NumberValue Int
 deriving (Show,Eq)

type Environment = Map Name Value

emptyEnv :: Environment
emptyEnv = fromList []
