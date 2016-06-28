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
import Simulator.CallRecord
import Simulator.Heap

import Simulator.Rules.Base

assignJob :: Rule
assignJob acqua =
    case (getAvailable (processingUnits acqua), firstOf (jobs (queue acqua))) of
      (Just pu, Just job) -> trace ((show (PU.puId pu)) ++ ": assignJob ") $ Acqua bb q' pus' i' ff s'
        where
          Acqua bb q pus i ff s = acqua
          pus' = updatePU pus pu'
          pId = PU.puId pu
          env = environments pu
          hp = heap pu
          ra = returnAddrs pu
          cc = callCount pu

          BB _ _ c t = lookupBB bb l

          Job l envId pId' callRec x = job
          Queue js qlck = q
          jobs' = List.delete job js

          -- init env
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId x) ra
          cc' = Map.insert newEnvId 0 cc

          -- add message
          m = MsgReqEnv pId newEnvId pId' envId callRec
          i' = (ConstMsgReqEnv m) : i

          -- add callRecord on heap
          hpPos = Map.size hp
          hp' = Map.insert hpPos (CallRecordV (CallRecord "receivedCallRecord" 0 0 emptyParams)) hp
          emptyParams = Seq.replicate 5 (NumberV 0)

          -- create env with pointer to callRecord
          pt = PointerV $ Pointer pId hpPos
          nenv = Map.fromList [("callRecord", pt)]
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
      (_,_) -> acqua

firstOf :: [Job] -> Maybe Job
firstOf [] = Nothing
firstOf q = Just (head q)

getAvailable :: [ProcessingUnit] -> Maybe ProcessingUnit
getAvailable [] = Nothing
getAvailable (pu:pus') =
  if (PU.terminator pu) == Empty && (PU.locked pu) == False && (PU.puId pu) > 0
    then Just pu
    else getAvailable pus'

