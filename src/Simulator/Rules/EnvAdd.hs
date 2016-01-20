module Simulator.Rules.EnvAdd where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules

envAdd :: Rule
envAdd (Acqua bb q pus i f s) =
    Acqua bb q (map stepEnvAdd pus) i f s
  where
    stepEnvAdd pu =
      case PU.commands pu of
        ((EnvAddL envId x1 x2):cs) -> if PU.tainted pu == False
                                   then trace ((show (PU.puId pu)) ++  ": newAdd") pu'
                                   else pu
          where
            PU pId _ t ce rEnv cEnv ra cc se _ = pu
            Just cenv = Map.lookup ce rEnv
            Just cenv' = Map.lookup envId cEnv
            Just val = Map.lookup x2 cenv
            cenv'' = case val of
                      (NumberValue n) -> Map.insert x1 (NumberValue n) cenv'
                      (LabelValue l) -> Map.insert x1 (LabelValue l) cenv'
            cEnv' = Map.insert envId cenv'' cEnv
            pu' = PU pId cs t ce rEnv cEnv' ra cc se True
        _ -> pu

