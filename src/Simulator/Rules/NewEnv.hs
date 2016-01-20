module Simulator.Rules.NewEnv where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules

envNew :: Rule
envNew (Acqua bb q pus i f s) =
    Acqua bb q pus1 i f s1
  where
    (pus1, s1) = stepNewEnv pus s

    stepNewEnv [] s = ([],s)
    stepNewEnv (pu:pus) s =
      case (PU.commands pu,PU.tainted pu) of
        ((EnvNew envId _):cs, False) -> trace ((show (PU.puId pu)) ++  ": newEnv") $ (pu1:pus2,s2)
          where
            (pus3,s3) = (stepNewEnv pus s)
            (pus2,s2) = (stepNewEnv pus s1)
            PU pId _ t ce rEnv cEnv ra cc se _ = pu
            (newEnvId,s1) = getNextEnvId s envId
            -- cEnv1 = Map.insert envId emptyEnv cEnv
            Just cenv = Map.lookup ce rEnv
            cEnv1 = Map.insert newEnvId cenv cEnv
            pu1 = PU pId cs t ce rEnv cEnv1 ra cc se True
        _ -> (pu:pus3,s3)
          where
            (pus3,s3) = (stepNewEnv pus s)

getNextEnvId :: Map.Map String StateValue -> String -> (EnvId, Map.Map String StateValue)
getNextEnvId s envId = (nextEnvId,s1')
  where
    Just (IntVal count) = Map.lookup "envId" s
    s1 = Map.insert "envId" (IntVal (count+1)) s
    Just (NewEnvIds env_map) = Map.lookup "newEnvIds" s
    env_map' = Map.insert envId nextEnvId env_map 
    s1' = Map.insert "newEnvIds" (NewEnvIds env_map') s1
    nextEnvId = "newEnvIdsenv_" ++ (show count)
