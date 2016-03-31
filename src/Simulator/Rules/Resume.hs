module Simulator.Rules.Resume where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base


resume :: Rule
resume (Acqua bb q pus i f s) =
    Acqua bb q (map stepResume pus) i f s
  where
    stepResume pu =
      let
        t = PU.terminator pu
        callCounts = Map.toList (PU.callCount pu)
        zeroedCallCount cc = case cc of
          ((k,v):xs) -> if v == 0
                          then Just k
                          else zeroedCallCount xs
          [] -> Nothing
      in
        case (t,(zeroedCallCount callCounts), PU.locked pu) of
          (Empty,Just k, False) -> trace ((show (PU.puId pu)) ++ ": resuming")  $ pu'
            where
              PU pId _ _ _ rEnv cEnv ra cc se _ enbl = pu
              Just (ExecutionContext c' t') = traceShow (k,se) $ Map.lookup k se
              pu' = PU pId c' t' k rEnv cEnv ra cc se True enbl
          _ -> pu
