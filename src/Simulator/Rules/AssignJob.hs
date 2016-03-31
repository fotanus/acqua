module Simulator.Rules.AssignJob where

import Text.Show.Pretty
import Data.List as List
import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Environment

import Simulator.Rules.Base

assignJob :: Rule
assignJob acqua =
  let
    Acqua bb q pus i ff s = acqua
  in
    case (getAvailable pus, firstOf (jobs q)) of
      (Just pu, Just job) ->
        trace ((show (PU.puId pu)) ++ ": assignJob " ++ (ppShow job)) $ Acqua bb q' pus' i ff s'
        where
          pus' = updatePU pus p'
          PU pId _ _ _ rEnv cEnv ra cc se _ = pu
          Job l envId pId' envId'' x = job
          BB _ n c t = getBB l bb
          Just ce = copyEnv pus pId' envId n
          rEnv' = Map.insert newEnvId ce rEnv 
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId'' x) ra
          cc' = Map.insert newEnvId 0 cc
          Queue js lck = q
          jobs' = List.delete job js
          q' = Queue jobs' lck
          p' = PU pId c t newEnvId rEnv' cEnv ra' cc' se True
      (_,_)-> acqua


getNextEnvId :: Map.Map String StateValue -> (EnvId, Map.Map String StateValue)
getNextEnvId s = (nextEnvId,s')
  where
    Just (IntVal count) = Map.lookup "envId" s
    s' = Map.insert "envId" (IntVal (count+1)) s
    nextEnvId = "env_" ++ (show count)

firstOf :: [Job] -> Maybe Job
firstOf [] = Nothing
firstOf q = Just (head q)

copyEnv :: [ProcessingUnit] -> PId -> EnvId -> Int -> Maybe Environment
copyEnv [] _ _ _  = Nothing
copyEnv (pu:pus) pId' envId n =
  if (PU.puId pu) == pId' 
    then Map.lookup envId (copyEnvs pu)
    else copyEnv pus pId' envId n


getAvailable :: [ProcessingUnit] -> Maybe ProcessingUnit
getAvailable [] = Nothing
getAvailable (pu:pus') =
  if (PU.terminator pu) == Empty && (PU.locked pu) == False && (PU.puId pu) > 0
    then Just pu
    else getAvailable pus'

