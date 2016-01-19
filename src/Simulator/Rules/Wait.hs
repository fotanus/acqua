module Simulator.Rules.Wait where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules

wait :: Rule
wait (Acqua bb q pus i f s) =
    Acqua bb q (map stepWait pus) i f s
  where
    stepWait pu =
      case PU.commands pu of
        (Wait:cs) -> if PU.tainted pu == False
                       then trace ((show (PU.puId pu)) ++ ": Wait" ) pu'
                       else pu
          where
            PU pId _ t ce rEnv cEnv ra cc se _ = pu
            se' = Map.insert ce (ExecutionContext cs t) se
            pu' = PU pId [] Empty ce rEnv cEnv ra cc se' True
        _ -> pu

