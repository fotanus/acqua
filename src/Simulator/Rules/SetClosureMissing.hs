module Simulator.Rules.SetClosureMissing where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureMissing :: Rule
setClosureMissing (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureMissing pus) i f s
  where
    stepSetClosureMissing pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureMissing x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureMissing " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ClosureV closur) = Map.lookup (addr pointer) hp
            Just (NumberV num) = Map.lookup n cenv
            closur' = closur { paramMissing = num }
            hp' = Map.insert (addr pointer) (ClosureV closur') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
