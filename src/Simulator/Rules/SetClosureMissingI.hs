module Simulator.Rules.SetClosureMissingI where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureMissingI :: Rule
setClosureMissingI (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureMissingI pus) i f s
  where
    stepSetClosureMissingI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureMissingI x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureMissingI " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ClosureV closure) = Map.lookup (addr pointer) hp
            closure' = closure { paramMissing = n }
            hp' = Map.insert (addr pointer) (ClosureV closure') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
