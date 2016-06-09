module Simulator.Rules.SetClosureCount where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap as Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureCount :: Rule
setClosureCount (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureCount pus) i f s
  where
    stepSetClosureCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            hp = heap pu
            PointerV pointer = getVal pu x
            NumberV num = getVal pu n
            ClosureV closur = Heap.lookupPt pointer hp
            closur' = closur { paramCount = num }
            hp' = Map.insert (addr pointer) (ClosureV closur') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
