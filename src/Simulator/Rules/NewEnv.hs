module Simulator.Rules.NewEnv where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules

envNew :: Rule
envNew (Acqua bb q pus i f) =
    Acqua bb q (map stepNewEnv pus) i f
  where
    stepNewEnv pu =
      case PU.commands pu of
        ((EnvNew envId n):cs) -> if PU.tainted pu == False
                                   then trace ((show (PU.puId pu)) ++  ": newEnv") pu'
                                   else pu
          where
            PU pId _ t ce rEnv cEnv ra cc se _ = pu
            Just cenv = Map.lookup ce rEnv
            cEnv' = Map.insert envId cenv cEnv
            pu' = PU pId cs t ce rEnv cEnv' ra cc se True
        _ -> pu

