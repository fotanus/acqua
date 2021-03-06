module Simulator.Rules.Resume where

import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base


resume :: Rule
resume acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepResume (processingUnits acqua)
    stepResume pu =
      let
        callCounts = Map.toList (PU.callCount pu)
        zeroedCallCount cc = case cc of
          ((k,v):xs) -> if v == 0
                          then Just k
                          else zeroedCallCount xs
          [] -> Nothing
      in
        case ((PU.free pu),(zeroedCallCount callCounts), PU.locked pu) of
          (True, Just k, False) -> trace ((show (PU.puId pu)) ++ ": resuming" ++ (show k))  $ pu'
            where
              se = sleepingExecution pu
              Just (ExecutionContext c' t') = Map.lookup k se
              pu' = pu { PU.free = False, PU.commands = c', currentEnv = k, PU.terminator = t', locked = True }
          _ -> pu
