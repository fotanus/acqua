module Simulator.Rules.SetClosureParam where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

setClosureParam :: Rule
setClosureParam (Acqua bb q pus i f s) =
    Acqua bb q (map stepSetClosureParam pus) i f s
  where
    stepSetClosureParam pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureParam x i v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureParam " ++ (show x) ++ " " ++ (show i) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu

            Just cenv = Map.lookup ce envs
            Just (ClosureV closure) = Map.lookup x cenv
            Just (BaseValV (NumberV idx)) = Map.lookup i cenv
            Just (BaseValV val) = Map.lookup v cenv
            params' = Sequence.update (idx+1) val (params closure) -- idx from first pos is 1, not 0, so we sum
            closure' = closure { params = (traceShowId params') }
            cenv' = Map.insert x (ClosureV closure') cenv
            envs' = Map.insert ce cenv' envs
            pu' = pu { PU.commands = cs, environments = envs', locked = True }
        _ -> pu