module Simulator.Rules.AssignJob where

import Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import AcquaIR.Language
import Simulator.Acqua
import Simulator.AcquaState
import Simulator.Job as J
import Simulator.ProcessingUnit as PU
import Simulator.Queue as Q
import Simulator.Interconnection
import Simulator.Value as VAL
import Simulator.List as LIST
import Simulator.CallRecord as CR
import Simulator.CallRecordSeg as CRS

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
          (srcCachePtr, trgCachePtr) = callRecordCache pu

          callRecPtr = case source of
                CallSource callRec -> callRec
                MapSource callRec _ -> callRec
                SMapSource callRec _ _ -> callRec

          Job pId' source sourceSize envId x _ = job
          Queue js qlck = q
          jobs' = List.delete job js

          -- init env
          (newEnvId,s') = getNextEnvId s
          ra' = Map.insert newEnvId (ReturnAddr pId' envId x) ra
          cc' = Map.insert newEnvId 0 cc

          -- add message
          m =  MsgReqJobCallRecord pId pt pId' callRecPtr
          i' = if canUseCache
               then i
               else (ConstMsgReqJobCallRecord m (msgStepsToPropagate acqua)) : i

          -- check if can reuse cache
          canUseCache = case (J.isMap job, srcCachePtr == callRecPtr, Map.lookup (addr trgCachePtr) crseg) of
                              (True, True, Just (CallRecordV _)) -> case Map.lookup "opt" (acquaState acqua) of
                                                                   Just (IntVal 0) -> False
                                                                   Just (IntVal 2) -> False
                                                                   _        ->  True
                              _                             -> False

          Just (CallRecordV cachecr) = Map.lookup (addr trgCachePtr) crseg

          -- prepare callrecord to be used
          newCallRecord = if canUseCache
                          then let
                                  v = case source of 
                                      MapSource _ v' -> v'
                                      SMapSource _ pointr n -> v'
                                                where
                                                  -- todo: copy through interconnection
                                                  pusrc = (processingUnits acqua)!!(VAL.puId pointr)
                                                  v' = case CRS.lookup (addr pointr) (callRecordSeg pusrc) of
                                                      ListV l -> (LIST.params l)!!n
                                                      _ -> error "not a list on smap"
                                in
                                  cachecr { CR.params = Seq.update (sourceSize-1) v (CR.params cachecr), timeout = maxTimeout }
                          else emptyCallRecord { functionName = "receivedCallRecord", CR.params = defaultParams }

          defaultParams = case source of
                          CallSource _ -> Seq.replicate sourceSize (NumberV 0)
                          MapSource _ v -> Seq.update (sourceSize-1) v $ Seq.replicate sourceSize (NumberV 0)
                          SMapSource _ pointr n -> Seq.update (sourceSize-1) v $ Seq.replicate sourceSize (NumberV 0)
                                                where
                                                  -- todo: copy through interconnection
                                                  pusrc = (processingUnits acqua)!!(VAL.puId pointr)
                                                  v = case CRS.lookup (addr pointr) (callRecordSeg pusrc) of
                                                      ListV l -> (LIST.params l)!!n
                                                      _ -> error "not a list on smap"

          -- add callRecord on callRecordSeg if created a new
          crseg' = if canUseCache
                     then traceShow "Using cached pointer" Map.insert (addr trgCachePtr) (CallRecordV newCallRecord) crseg
                     else traceShow "Can't use cached pointer" $ Map.insert crsegPos (CallRecordV newCallRecord) crseg
          crsegPos = if canUseCache
                      then addr trgCachePtr
                      else nextFreePos crseg

          -- create env with pointer to callRecord
          pt = Pointer pId crsegPos
          ptv = PointerV pt
          nenv = Map.fromList [("callRecord", ptv)]
          nenv' = if J.isMap job
                  then Map.insert "isMap" (NumberV 1) nenv
                  else Map.insert "isMap" (NumberV 0) nenv
          nenvcache = Map.fromList [("callRecord", (PointerV trgCachePtr))]
          nenvcache' = if J.isMap job
                       then Map.insert "isMap" (NumberV 1) nenvcache
                       else Map.insert "isMap" (NumberV 0) nenvcache
          
          env' = if canUseCache
                 then Map.insert newEnvId nenvcache' env
                 else Map.insert newEnvId nenv' env

          -- if is a map, cache the call record
          crc  = if (J.isMap job)
                 then (callRecPtr, pt)
                 else (callRecordCache pu)

          -- fetch basic block if the callrecord was cached
          cmds = if canUseCache then c' else (PU.commands pu)
          term = if canUseCache then t' else (PU.terminator pu)
          BB _ _ c' t' = lookupBB (program acqua) (functionName newCallRecord)

          q' = Queue jobs' qlck
          pu' = pu {
            PU.commands = cmds,
            PU.terminator = term, 
            free = False,
            currentEnv = newEnvId,
            environments = env',
            callRecordSeg = crseg',
            returnAddrs = ra',
            callCount = cc',
            callRecordCache = crc,
            enabled = canUseCache,
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

