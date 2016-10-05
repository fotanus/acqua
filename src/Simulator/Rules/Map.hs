module Simulator.Rules.Map where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq 
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.Queue as Q
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.CallRecord
import Simulator.Value
import Simulator.List
import Simulator.ReturnAddrVar

import Simulator.Rules.Base

mapRule :: Rule
mapRule acqua =
    acqua { queue = q', processingUnits = pus' }
  where
    (q', pus') = stepMapRule (queue acqua) (processingUnits acqua)

stepMapRule :: Queue -> [ProcessingUnit] -> (Queue, [ProcessingUnit])
stepMapRule q [] = (q,[])
stepMapRule q (pu:pus) =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Map x l1 l2):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = Map " ++ (show l1) ++ " " ++ (show l2)) (q'', pu':pus')
          where
            pId = PU.puId pu
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            -- get call record and list
            Just cenv = Map.lookup ce envs
            Just (PointerV pointer1) = Map.lookup l1 cenv
            Just (PointerV pointer2) = Map.lookup l2 cenv
            Just (CallRecordV callRec) = Map.lookup (addr pointer1) crseg
            Just (ListV (List _ paramsList)) = Map.lookup (addr pointer2) crseg

            -- create new list to hold the result
            crsegPos = nextFreePos crseg
            newPointer = Pointer (PU.puId pu) crsegPos
            nParams = length paramsList
            newList = List nParams (take nParams (repeat (NumberV 0)))
            crseg' = Map.insert crsegPos (ListV newList) crseg

            -- update call record count to deallocate
            crseg'' = Map.insert (addr pointer1) (CallRecordV (callRec { copiesToDelete = nParams} )) crseg'

            -- add new jobs to queue
            l = functionName callRec
            j = map (\(n,idx) -> let NumberV v = n in Job l ce pId (MapSource l1 v) (Seq.length (Simulator.CallRecord.params callRec)) (ListVal newPointer idx)) (zip paramsList [0..])
            q' = q { jobs = j ++ (jobs q) }

            -- increment call count
            cc  = PU.callCount pu
            Just nCalls = Map.lookup ce cc
            cc' = trace ("new calls waiting: " ++ (show (nCalls+nParams))) $ Map.insert ce (nCalls+nParams) cc

            pu' = (setVal pu x (PointerV newPointer)) { PU.commands = cs, PU.callCount = cc', callRecordSeg = crseg'', PU.locked = True, PU.stallCycles = nParams+1}

            (q'', pus') = stepMapRule q' pus
        _ -> (q', pu:pus')
          where (q', pus') = stepMapRule q pus
