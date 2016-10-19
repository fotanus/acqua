module Simulator.Rules.AssignJob where

import Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

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
      (Just pu, Just job) -> trace ((show (PU.puId pu)) ++ ": assignJob " ++ (show job)) $ acqua { queue = q', processingUnits = pus', interconnection =  i', acquaState = s' }
        where
          pus = processingUnits acqua
          i = interconnection acqua
          s = acquaState acqua
          q = queue acqua

          pus' = updatePU pus pu'
          pId = PU.puId pu
          env = environments pu
          crseg = callRecordSeg pu
          ra = returnAddrs pu
          cc = callCount pu

          Job pId' source sourceSize envId x = job
          Queue js qlck = q
          jobs' = List.delete job js

          -- init env
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId x) ra
          cc' = Map.insert newEnvId 0 cc

          -- add message
          m = case source of
                CallSource callRec -> MsgReqJobCallRecord pId pt pId' callRec
                MapSource callRec _ -> MsgReqJobCallRecord pId pt pId' callRec
          i' = case source of
                CallSource _ -> (ConstMsgReqJobCallRecord m (msgStepsToPropagate acqua)) : i
                MapSource _ _ -> (ConstMsgReqJobCallRecord m (msgStepsToPropagate acqua)) : i

          -- add callRecord on callRecordSeg
          crsegPos = nextFreePos crseg
          crseg' = Map.insert crsegPos (CallRecordV (emptyCallRecord { functionName = "receivedCallRecord", params = defaultParams })) crseg
          defaultParams = case source of
                          CallSource _ -> Seq.replicate sourceSize (NumberV 0)
                          MapSource _ v -> Seq.update (sourceSize-1) (NumberV v) $ Seq.replicate sourceSize (NumberV 0)

          -- create env with pointer to callRecord
          pt = Pointer pId crsegPos
          ptv = PointerV pt
          nenv = Map.fromList [("callRecord", ptv)]
          env' = Map.insert newEnvId nenv env


          q' = Queue jobs' qlck
          pu' = pu {
            free = False,
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
firstOf (j:_) = Just j

getAvailable :: [ProcessingUnit] -> Maybe ProcessingUnit
getAvailable [] = Nothing
getAvailable (pu:pus') =
  if (PU.free pu) && (PU.locked pu) == False
    then Just pu
    else getAvailable pus'

