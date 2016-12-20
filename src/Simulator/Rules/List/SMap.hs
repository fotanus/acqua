module Simulator.Rules.List.SMap where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq 
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.Job
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.CallRecord as CR
import Simulator.Value
import Simulator.List
import Simulator.ReturnAddrVar

import Simulator.Rules.Base

sMapRule :: Rule
sMapRule acqua =
    acqua { processingUnits = stepSMapRule (processingUnits acqua) }

stepSMapRule :: [ProcessingUnit] -> [ProcessingUnit]
stepSMapRule [] = []
stepSMapRule (pu:pus) =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SMap x l1 l2 l3):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = SMap " ++ (show l1) ++ " " ++ (show l2) ++ " " ++ (show l3)) (pu':pus')
          where
            pId = PU.puId pu
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            -- get call record and list
            Just cenv = Map.lookup ce envs
            Just (PointerV pointer1) = Map.lookup l1 cenv
            Just (PointerV pointer2) = Map.lookup l2 cenv
            Just (NumberV nParams) = Map.lookup l3 cenv
            Just (CallRecordV callRec) = Map.lookup (addr pointer1) crseg

            -- create new list to hold the result
            crsegPos = nextFreePos crseg
            newPointer = Pointer (PU.puId pu) crsegPos
            newList = List nParams (take nParams (repeat (NumberV 0)))
            crseg' = Map.insert crsegPos (ListV newList) crseg
            ocr = Map.insert (currentEnv pu) (Pointer pId (addr pointer1)) (originCallRec pu)

            -- update call record count to deallocate
            crseg'' = Map.insert (addr pointer1) (CallRecordV (callRec { CR.isMap = True, timeout = maxTimeout + 1} )) crseg'

            -- add new jobs to queue
            js = map (\idx -> Job pId (SMapSource pointer1 pointer2 idx) (Seq.length (CR.params callRec)) ce (ListVal newPointer idx) True) [0..(nParams-1)]
            ojq = js ++ (outgoingJobQueue pu)

            -- increment call count
            cc  = PU.callCount pu
            Just nCalls = Map.lookup ce cc
            cc' = trace ("new calls waiting: " ++ (show (nCalls+nParams))) $ Map.insert ce (nCalls+nParams) cc

            pu' = (setVal pu x (PointerV newPointer)) { PU.commands = cs, PU.callCount = cc', callRecordSeg = crseg'', PU.locked = True, PU.stallCycles = nParams+1, originCallRec = ocr, outgoingJobQueue = ojq }

            pus' = stepSMapRule pus
        _ -> pu:pus'
          where  pus' = stepSMapRule pus
