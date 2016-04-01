module Simulator.Rules.Wait where

import qualified Data.Map as Map
import Text.Show.Pretty
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base

wait :: Rule
wait (Acqua bb q pus i f s) =
    Acqua bb q (map stepWait pus) i f s
  where
    stepWait pu =
      case (PU.commands pu, PU.canExecuteCmds pu) of
        (Wait:cs, True) -> trace ((show (PU.puId pu)) ++ ": Wait\n" ++ (ppShow (copyEnvs pu))) pu'
          where
            PU pId _ t ce rEnv cEnv ra cc se omq enbl _ = pu
            se' = Map.insert ce (ExecutionContext cs t) se
            pu' = PU pId [] Empty ce rEnv cEnv ra cc se' omq enbl True
        _ -> pu

