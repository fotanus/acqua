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
      case (PU.commands pu,PU.terminator pu, PU.canExecuteCmds pu) of
        ([], Goto l, True) -> trace ((show (PU.puId pu)) ++ ": goto") pu'
         where
           BB _ _ c' t' = getBB l bb
           pu' = pu { PU.commands = c', PU.terminator = t', locked = True }
        _ -> pu



