module Simulator.Rules.CallL where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue as Q
import Simulator.Value
import Simulator.CallRecordSeg
import Simulator.CallRecord
import Simulator.ReturnAddrVar

import Simulator.Rules.Base

callL :: Rule
callL acqua =
    acqua { queue = q', processingUnits = pus' }
  where
    (q', pus') = stepCallL (queue acqua) (processingUnits acqua)

stepCallL :: Queue -> [ProcessingUnit] -> (Queue, [ProcessingUnit])
stepCallL q [] = (q,[])
stepCallL q (pu:pus) =
  case (PU.commands pu,PU.canExecuteCmds pu,Q.locked q) of
    ((CallL x1 idxname x2):cs,True,False) -> trace ((show (PU.puId pu)) ++  ": callL" ) (q'', pu':pus')
      where
        (q'', pus') = stepCallL q' pus

        -- set job on queue
        ce = PU.currentEnv pu
        pId = PU.puId pu
        envs = PU.environments pu
        crseg = PU.callRecordSeg pu
        Just cenv = Map.lookup ce envs
        Just (PointerV retPointer) = Map.lookup x1 cenv
        Just (PointerV pointer) = Map.lookup x2 cenv
        Just (NumberV idx) = Map.lookup idxname cenv
        Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg

        j = Job pId (CallSource pointer) (Seq.length (params callRec)) ce (ListVal retPointer idx) False
        q' = q { jobs = j:(jobs q) }

        -- increment calls count
        cc  = PU.callCount pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        ocr = Map.insert ce pointer (originCallRec pu)
        pu' = pu { PU.commands = cs, PU.callCount = cc', originCallRec = ocr, PU.locked = True }

    _ -> (q', pu:pus')
      where (q', pus') = stepCallL q pus
