module Simulator.Rules.Filter where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.Queue as Q
import Simulator.ProcessingUnit as PU
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

filterRule :: Rule
filterRule acqua =
    acqua { queue = q', processingUnits = pus' }
  where
    (q', pus') = stepFilterRule (queue acqua) (processingUnits acqua)

stepFilterRule :: Queue -> [ProcessingUnit] -> (Queue, [ProcessingUnit])
stepFilterRule q [] = (q,[])
stepFilterRule q (pu:pus) =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((Filter x l1 l2):cs),True) -> trace ((show (PU.puId pu)) ++ " " ++ (show x) ++ " = Filter " ++ (show l1) ++ " " ++ (show l2)) (q', pu':pus')
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            crseg = callRecordSeg pu

            -- get call record and list
            Just cenv = Map.lookup ce envs
            Just (PointerV pointer1) = Map.lookup l1 cenv
            Just (PointerV pointer2) = Map.lookup l2 cenv
            Just (ListV (List _ selectList)) = Map.lookup (addr pointer1) crseg
            Just (ListV (List _ paramsList)) = Map.lookup (addr pointer2) crseg
            resultList = map (\(_,p) -> p) $ filter (\(s,_) -> (numVal s) > 0) (zip selectList paramsList)

            -- create new list to hold the result
            crsegPos = nextFreePos crseg
            newPointer = Pointer (PU.puId pu) crsegPos
            nParams = length resultList
            newList = List nParams resultList
            crseg' = Map.insert crsegPos (ListV newList) crseg

            pu' = (setVal pu x (PointerV newPointer)) { PU.commands = cs, callRecordSeg = crseg', PU.locked = True }

            (q', pus') = stepFilterRule q pus
        _ -> (q', pu:pus')
          where (q', pus') = stepFilterRule q pus
