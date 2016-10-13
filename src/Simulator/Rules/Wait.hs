module Simulator.Rules.Wait where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base

wait :: Rule
wait acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepWait (processingUnits acqua)
    stepWait pu =
      case (PU.commands pu, PU.canExecuteCmds pu) of
        (Wait:cs, True) -> trace ((show (PU.puId pu)) ++ ": Wait") pu'
          where
            ce = currentEnv pu
            se = sleepingExecution pu
            t = PU.terminator pu
            se' = Map.insert ce (ExecutionContext cs t) se
            pu' = pu { PU.commands = [], PU.free = True, PU.terminator = Empty, sleepingExecution = se', locked = True }
        _ -> pu

