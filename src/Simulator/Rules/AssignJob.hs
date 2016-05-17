module Simulator.Rules.AssignJob where

import Data.List as List
import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Interconnection

import Simulator.Rules.Base

assignJob :: Rule
assignJob acqua =
  let
    Acqua bb q pus i ff s = acqua
  in
    case (getAvailable pus, firstOf (jobs q)) of
      (Just pu, Just job) ->
        trace ((show (PU.puId pu)) ++ ": assignJob ") $ Acqua bb q' pus' i' ff s'
        where
          pus' = updatePU pus p'
          PU pId _ _ _ env ra cc se omq _ _ = pu
          BB _ _ c t = getBB l bb

          Job l envId pId' envId'' x = job
          Queue js qlck = q
          jobs' = List.delete job js

          -- init env
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId'' x) ra
          cc' = Map.insert newEnvId 0 cc

          -- add message
          m = MsgReqEnv pId newEnvId pId' envId
          i' = (ConstMsgReqEnv m) : i

          q' = Queue jobs' qlck
          env' = Map.insert newEnvId (Map.fromList []) env
          p' = PU pId c t newEnvId env' ra' cc' se omq False True
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


getAvailable :: [ProcessingUnit] -> Maybe ProcessingUnit
getAvailable [] = Nothing
getAvailable (pu:pus') =
  if (PU.terminator pu) == Empty && (PU.locked pu) == False && (PU.puId pu) > 0
    then Just pu
    else getAvailable pus'

