module Simulator.Rules.AssignJob where

import Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Interconnection
import Simulator.Value
import Simulator.Closure
import Simulator.Environment
import Simulator.Heap

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
          pus' = updatePU pus pu'
          pId = PU.puId pu
          env = environments pu
          hp = heap pu
          ra = returnAddrs pu
          cc = callCount pu

          BB _ _ c t = getBB l bb

          Job l envId pId' closure x = job
          Queue js qlck = q
          jobs' = List.delete job js

          -- init env
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId x) ra
          cc' = Map.insert newEnvId 0 cc

          -- add message
          m = MsgReqEnv pId newEnvId pId' envId closure
          i' = (ConstMsgReqEnv m) : i

          -- add closure on heap
          hpPos = Map.size hp
          hp' = Map.insert hpPos (ClosureV (Closure "receivedClosure" 0 0 emptyParams)) hp
          emptyParams = Seq.replicate 2 (NumberV 0)

          -- create env with pointer to closure
          pt = PointerV $ Pointer pId hpPos
          nenv = Map.fromList [("closure", pt)]
          env' = Map.insert newEnvId nenv env

          q' = Queue jobs' qlck
          pu' = pu {
            PU.commands = c,
            PU.terminator = t,
            currentEnv = newEnvId,
            environments = env',
            heap = hp',
            returnAddrs = ra',
            callCount = cc',
            enabled = False,
            PU.locked = True
          }
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

