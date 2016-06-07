module Simulator.Rules.GetClosureCount where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

getClosureCount :: Rule
getClosureCount (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetClosureCount pus) i f s
  where
    stepGetClosureCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ClosureV closure) = Map.lookup (addr pointer) (heap pu)
            count = (paramCount closure)
            cenv' = Map.insert n (NumberV count) cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
