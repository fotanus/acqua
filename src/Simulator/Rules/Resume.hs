module Simulator.Rules.Resume where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules


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
        case (t,(zeroedCallCount callCounts)) of
          (Empty,Just k) -> if PU.tainted pu == False
                              then trace  ((show (PU.puId pu)) ++ ": resuming")  $ pu'
                              else pu
            where
              PU pId _ _ _ rEnv cEnv ra cc se _ = pu
              Just (ExecutionContext c' t') = traceShow (k,se) $ Map.lookup k se
              pu' = PU pId c' t' k rEnv cEnv ra cc se True
          (_, _) -> pu
