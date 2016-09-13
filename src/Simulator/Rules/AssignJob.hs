module Simulator.Rules.AssignJob where

import Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.AcquaState
import Simulator.ProcessingUnit as PU
import Simulator.Queue
import Simulator.Interconnection
import Simulator.Value
import Simulator.CallRecord
import Simulator.CallRecordSeg

import Simulator.Rules.Base

assignJob :: Rule
assignJob acqua =
    case (getAvailable (processingUnits acqua), firstOf (jobs (queue acqua))) of
      (Just pu, Just job) -> trace ((show (PU.puId pu)) ++ ": assignJob ") $ acqua { queue = q', processingUnits = pus', interconnection =  i', acquaState = s' }
        where
          pus = processingUnits acqua
          bb = program acqua
          i = interconnection acqua
          s = acquaState acqua
          q = queue acqua

          pus' = updatePU pus pu'
          pId = PU.puId pu
          env = environments pu
          crseg = callRecordSeg pu
          ra = returnAddrs pu
          cc = callCount pu

          BB _ _ c t = lookupBB bb l

          Job l envId pId' source sourceSize x = job
          Queue js qlck = q
          jobs' = List.delete job js

          -- init env
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId x) ra
          cc' = Map.insert newEnvId 0 cc

          -- add message
          m = case source of
                CallSource callRec -> MsgReqEnv pId newEnvId pId' envId callRec
                MapSource callRec _ -> MsgReqEnv pId newEnvId pId' envId callRec
          i' = (ConstMsgReqEnv m (msgStepsToPropagate acqua)) : i

          -- add callRecord on callRecordSeg
          crsegPos = nextFreePos crseg
          crseg' = Map.insert crsegPos (CallRecordV (CallRecord "receivedCallRecord" 0 0 defaultParams)) crseg
          defaultParams = case source of
                          CallSource _ -> Seq.replicate sourceSize (NumberV 0)
                          MapSource _ v -> Seq.update (sourceSize-1) (NumberV v) $ Seq.replicate sourceSize (NumberV 0)

          -- create env with pointer to callRecord
          pt = PointerV $ Pointer pId crsegPos
          nenv = Map.fromList [("callRecord", pt)]
          env' = Map.insert newEnvId nenv env

          q' = Queue jobs' qlck
          pu' = pu {
            PU.commands = c,
            PU.terminator = t,
            currentEnv = newEnvId,
            environments = env',
            callRecordSeg = crseg',
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

