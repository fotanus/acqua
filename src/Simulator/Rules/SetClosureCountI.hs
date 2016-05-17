module Simulator.Rules.SetClosureCountI where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

setClosureCountI :: Rule
setClosureCountI (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureCountI pus) i f s
  where
    stepSetClosureCountI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureCountI x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureCountI " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            closure' = closure { paramCount = n }
            cenv' = Map.insert x (ClosureV closure') cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
