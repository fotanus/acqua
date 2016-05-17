module Simulator.Environment where

import Data.Map as Map
import Data.Sequence

import AcquaIR.Language

data BaseVal
  = LabelV  Label
  | NumberV Int
  deriving (Show,Eq)

data Closure = Closure {
  functionName :: Name,
  paramMissing :: Int,
  paramCount :: Int,
  params :: Seq BaseVal
} deriving (Show,Eq)

data Value
 = BaseValV BaseVal
 | ClosureV Closure
 deriving (Show,Eq)

type Environment = Map Name Value

emptyEnv :: Environment
emptyEnv = Map.fromList []
