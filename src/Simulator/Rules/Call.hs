module Simulator.Rules.Call where

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

call :: Rule
call acqua =
    acqua { queue = q', processingUnits = pus' }
  where
    (q', pus') = stepCall (queue acqua) (processingUnits acqua)

stepCall :: Queue -> [ProcessingUnit] -> (Queue, [ProcessingUnit])
stepCall q [] = (q,[])
stepCall q (pu:pus) =
  case (PU.commands pu,PU.canExecuteCmds pu,Q.locked q) of
    ((Call x1 x2):cs,True,False) -> trace ((show (PU.puId pu)) ++  ": call" ) (q'', pu':pus')
      where
        (q'', pus') = stepCall q' pus

        -- set job on queue
        ce = PU.currentEnv pu
        pId = PU.puId pu
        envs = PU.environments pu
        crseg = PU.callRecordSeg pu
        Just cenv = Map.lookup ce envs
        Just (PointerV pointer) = Map.lookup x2 cenv
        Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg

        j = Job pId (CallSource pointer) (Seq.length (params callRec)) ce (EnvVal x1)
        q' = q { jobs = j:(jobs q) }

        -- increment calls count
        cc  = PU.callCount pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        ocr = Map.insert ce pointer (originCallRec pu)
        pu' = pu { PU.commands = cs, PU.callCount = cc', originCallRec = ocr, PU.locked = True }

    _ -> (q', pu:pus')
      where (q', pus') = stepCall q pus
