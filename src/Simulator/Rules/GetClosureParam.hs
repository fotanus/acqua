module Simulator.Rules.GetClosureParam where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

getClosureParam :: Rule
getClosureParam (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetClosureParam pus) i f s
  where
    stepGetClosureParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureParam x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureParam " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ClosureV closure) = Map.lookup (addr pointer) hp
            val = Sequence.index (params closure) i
            cenv' = Map.insert v val cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
