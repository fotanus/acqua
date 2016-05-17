module Simulator.Rules.SetClosureMissingI where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

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

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            closure' = closure { paramMissing = n }
            cenv' = Map.insert x (ClosureV closure') cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
