module Simulator.Heap where

import Data.Map as Map

import Simulator.Value
import Simulator.Closure

type Heap = Map HeapAddr HeapValue

data HeapValue
  = ClosureV Closure
  deriving (Show,Eq)

nextFreePos :: Heap -> HeapAddr
nextFreePos heap = Map.size heap

lookup :: HeapAddr -> Heap -> HeapValue
lookup ad hp =
  let
    Just v = Map.lookup ad hp
  in
    v

lookupPt :: Pointer -> Heap -> HeapValue
lookupPt pt hp =
  let
    Just v = Map.lookup (addr pt) hp
  in
    v
