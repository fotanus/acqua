module Simulator.Rules.Call where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Queue as Q
import Simulator.Value
import Simulator.Heap
import Simulator.Closure

import Simulator.Rules.Base

call :: Rule
call (Acqua bb q pus i f s) =
    Acqua bb q' pus' i f s
  where
    (q', pus') = stepCall q pus s

stepCall :: Queue -> [ProcessingUnit] -> Map.Map String StateValue -> (Queue, [ProcessingUnit])
stepCall q [] _ = (q,[])
stepCall q (pu:pus) s =
  case (PU.commands pu,PU.canExecuteCmds pu,Q.locked q) of
    ((Call x1 x2):cs,True,False) -> trace ((show (PU.puId pu)) ++  ": call" ) (q'', pu':pus')
      where
        (q'', pus') = stepCall q' pus s

        -- set job on queue
        ce = PU.currentEnv pu
        pId = PU.puId pu
        envs = PU.environments pu
        hp = PU.heap pu
        Just cenv = Map.lookup ce envs
        Just (PointerV pointer) = Map.lookup x2 cenv
        Just (ClosureV closur) = Map.lookup (addr pointer) hp
        l = functionName closur
        j = Job l ce pId x2 x1
        q' = q { jobs = j:(jobs q) }

        -- increment calls count
        cc  = PU.callCount pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        pu' = pu { PU.commands = cs, PU.callCount = cc', PU.locked = True }

    _ -> (q', pu:pus')
      where (q', pus') = stepCall q pus s
