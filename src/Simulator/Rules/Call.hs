module Simulator.Rules.Call where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue as Q
import Simulator.Value
import Simulator.CallRecordSeg
import Simulator.CallRecord

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
        l = functionName callRec
        j = Job l ce pId x2 x1
        q' = q { jobs = j:(jobs q) }

        -- increment calls count
        cc  = PU.callCount pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        pu' = pu { PU.commands = cs, PU.callCount = cc', PU.locked = True }

    _ -> (q', pu:pus')
      where (q', pus') = stepCall q pus
