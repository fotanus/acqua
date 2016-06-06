module Simulator.Closure where

import Data.Sequence

import AcquaIR.Language
import Simulator.Value

data Closure = Closure {
  functionName :: Name,
  paramMissing :: Int,
  paramCount :: Int,
  params :: Seq Value
} deriving (Show,Eq)

emptyClosure :: Closure
emptyClosure = Closure "" 0 0 (fromList [])
