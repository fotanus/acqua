module Simulator.Rules.Assign where

import Data.List as List
import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Environment

import Simulator.Rules

assign :: Rule
assign acqua = 
  let
    Acqua bb q pus i ff = acqua
  in
    case (getAvailable pus, firstOf q) of
      (Just pu, Just job) ->
        trace "assign" $ Acqua bb q' pus' i ff
        where
          pus' = updatePU pus p'
          PU pId _ _ _ rEnv cEnv ra cc se = pu
          Job l envId pId' envId'' x = job
          BB _ n c t = getBB l bb
          Just ce = copyEnv pus pId' envId n
          rEnv' = Map.insert newEnvId ce rEnv 
          newEnvId = envId -- fixme
          ra' = Map.insert newEnvId (ReturnAddr pId' envId'' x) ra
          cc' = Map.insert newEnvId 0 cc
          q' = List.delete job q
          p' = PU pId c t newEnvId rEnv' cEnv ra' cc' se
      (_,_)-> acqua

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
  if (PU.terminator pu) == Empty
    then Just pu
    else getAvailable pus'

getBB :: Label -> Program -> BasicBlock
getBB l [] = error $ "Basic block not found: " ++ l
getBB l (bb:bbs) = 
  if (IR.label bb) == l
    then bb
    else getBB l bbs

updatePU :: [ProcessingUnit] -> ProcessingUnit -> [ProcessingUnit]
updatePU [] p' = error $ "Trying to update an unexisting PU: " ++ (show p')
updatePU (x:xs) p' =
  if (PU.puId x) == (PU.puId p')
    then p' : xs
    else x : updatePU xs p'
