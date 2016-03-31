module Simulator.Rules.Goto where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU

import Simulator.Rules.Base

goto :: Rule
goto (Acqua bb q pus i f s) = Acqua bb q (map executeGoto pus) i f s
  where
    executeGoto pu =
      case (PU.commands pu,PU.terminator pu, PU.locked pu) of
        ([], Goto l, False) -> trace ((show (PU.puId pu)) ++ ": goto") pu'
         where

           PU pId _ _ ce rEnv cEnv ra cc se _ enbl = pu
           BB _ _ c' t' = getBB l bb
           pu' = PU pId c' t' ce rEnv cEnv ra cc se True enbl
        _ -> pu



