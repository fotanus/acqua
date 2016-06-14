module Simulator.Rules.SetClosureCountI where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureCountI :: Rule
setClosureCountI acqua =
    acqua { processingUnits = newPus }
  where
    newPus = map stepSetClosureCountI (processingUnits acqua)
    stepSetClosureCountI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureCountI x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureCountI " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ClosureV closure) = Map.lookup (addr pointer) hp
            closure' = closure { paramCount = n }
            hp' = Map.insert (addr pointer) (ClosureV closure') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
