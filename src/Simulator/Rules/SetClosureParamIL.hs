module Simulator.Rules.SetClosureParamIL where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureParamIL :: Rule
setClosureParamIL (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureParamIL pus) i f s
  where
    stepSetClosureParamIL pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureParamIL x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureParamIL " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            params' = Sequence.update i (LabelV v) (params closure)
            closure' = closure { params = params' }
            cenv' = Map.insert x (ClosureV closure') cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu
