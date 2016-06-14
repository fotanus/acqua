module Simulator.Rules.SetClosureParamI where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Heap
import Simulator.Value
import Simulator.Closure

import Simulator.Rules.Base

setClosureParamI :: Rule
setClosureParamI acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepSetClosureParamI (processingUnits acqua)
    stepSetClosureParamI pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((SetClosureParamI x idx v):cs),True) -> trace ((show (PU.puId pu)) ++ ": SetClosureParamI " ++ (show x) ++ " " ++ (show idx) ++ " " ++ (show v)) pu'
          where
            ce = PU.currentEnv pu
            envs = PU.environments pu
            hp = heap pu

            Just cenv = Map.lookup ce envs
            Just (PointerV pointer) = Map.lookup x cenv
            Just (ClosureV closure) = Map.lookup (addr pointer) hp
            Just val = traceShow cenv $ Map.lookup v cenv
            params' = Sequence.update idx val (params closure)
            closure' = closure { params = params' }
            hp' = Map.insert (addr pointer) (ClosureV closure') hp
            pu' = pu { PU.commands = cs, heap = hp', locked = True }
        _ -> pu
