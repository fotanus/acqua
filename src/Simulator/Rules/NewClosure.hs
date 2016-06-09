module Simulator.Rules.NewClosure where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.Heap as Heap
import Simulator.Closure

import Simulator.Rules.Base

newClosure :: Rule
newClosure (Acqua bb q pus i f s) =
    Acqua bb q (map stepNewClosure pus) i f s
  where
    stepNewClosure pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((NewClosure x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": NewClosure " ++ (show x) ++ " " ++ (show n)) pu'
          where
            hp = heap pu
            hpPos = Heap.nextFreePos hp
            pointer = Pointer (PU.puId pu) hpPos
            clos = Closure "" 0 0 (Sequence.replicate n (NumberV 0))
            hp' = Map.insert hpPos (ClosureV clos) hp
            pu' = (setVal pu x (PointerV pointer)) { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
