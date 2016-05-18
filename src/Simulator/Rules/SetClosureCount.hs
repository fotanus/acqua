module Simulator.Rules.SetClosureCount where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

setClosureCount :: Rule
setClosureCount (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureCount pus) i f s
  where
    stepSetClosureCount pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureCount x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureCount " ++ (show x) ++ " " ++ (show n)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            Just (BaseValV (NumberV num)) = Map.lookup n cenv
            closure' = closure { paramCount = num }
            cenv' = Map.insert x (ClosureV closure') cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
