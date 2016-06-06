module Simulator.Rules.SetClosureFn where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureFn :: Rule
setClosureFn (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureFn pus) i f s
  where
    stepSetClosureFn pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureFn x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureFn " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            closure' = closure { functionName = n }
            cenv' = Map.insert x (ClosureV closure') cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
