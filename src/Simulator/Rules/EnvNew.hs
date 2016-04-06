module Simulator.Rules.EnvNew where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base

envNew :: Rule
envNew (Acqua bb q pus i f s) =
    Acqua bb q pus1 i f s1
  where
    (pus1, s1) = stepNewEnv pus s

stepNewEnv :: [ProcessingUnit] -> AcquaState -> ([ProcessingUnit], AcquaState)
stepNewEnv [] s = ([],s)
stepNewEnv (pu:pus) s =
  case (PU.commands pu,PU.canExecuteCmds pu) of
    ((EnvNew envId _):cs, True) -> trace ((show (PU.puId pu)) ++  ": EnvNew" ++ envId ++ " as " ++ newEnvId) $ (pu1:pus2,s2)
      where
        (pus2,s2) = (stepNewEnv pus s1)
        PU pId _ t ce rEnv cEnv ra cc se omq enbl _ = pu
        (newEnvId,s1) = getNextEnvId s envId pId
        cEnv1 = Map.insert newEnvId (Map.fromList []) cEnv
        Just cenv = Map.lookup ce rEnv
        pu1 = PU pId cs t ce rEnv cEnv1 ra cc se omq enbl True
    _ -> (pu:pus3,s3)
      where
        (pus3,s3) = (stepNewEnv pus s)

getNextEnvId :: Map.Map String StateValue -> String -> Int -> (EnvId, Map.Map String StateValue)
getNextEnvId s envId pId = (nextEnvId,s1')
  where
    Just (IntVal count) = Map.lookup "envId" s
    s1 = Map.insert "envId" (IntVal (count+1)) s
    Just (NewEnvIds env_map) = Map.lookup "newEnvIds" s
    env_map' = Map.insert (pId,envId) nextEnvId env_map 
    s1' = Map.insert "newEnvIds" (NewEnvIds env_map') s1
    nextEnvId = "newEnvIdsenv_" ++ (show count)
