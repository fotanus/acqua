module Simulator.Rules.GetClosureMissing where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

getClosureMissing :: Rule
getClosureMissing (Acqua bb q pus i f s) =
    Acqua bb q (map stepGetClosureMissing pus) i f s
  where
    stepGetClosureMissing pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((GetClosureMissing x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": GetClosureMissing " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            missing = (paramMissing closure)
            cenv' = Map.insert n (BaseValV (NumberV missing)) cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
