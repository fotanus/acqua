module Simulator.Rules.EnvAdd where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

envAdd :: Rule
envAdd (Acqua bb q pus i f s) =
    Acqua bb q (map stepEnvAdd pus) i f s
  where
    stepEnvAdd pu =
      case (PU.commands pu, PU.canExecuteCmds pu) of
        ((EnvAddL envId x1 x2):cs, True) -> trace ((show (PU.puId pu)) ++  ": EnvAdd to env " ++ envId ++ " AKA " ++ copyEnvId) pu'
          where
            PU pId _ t ce rEnv cEnv ra cc se omq enbl _ = pu
            Just cenv = Map.lookup ce rEnv
            (Just (NewEnvIds envMap)) = Map.lookup "newEnvIds" s
            (Just copyEnvId) = Map.lookup (pId,envId) envMap
            Just cenv' = traceShow (pId,copyEnvId) $ Map.lookup copyEnvId cEnv
            Just val = Map.lookup x2 cenv
            cenv'' = case val of
                      (NumberValue n) -> Map.insert x1 (NumberValue n) cenv'
                      (LabelValue l) -> Map.insert x1 (LabelValue l) cenv'
            cEnv' = Map.insert copyEnvId cenv'' cEnv
            pu' = PU pId cs t ce rEnv cEnv' ra cc se omq enbl True 
        _ -> pu

