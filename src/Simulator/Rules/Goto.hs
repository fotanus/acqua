module Simulator.Rules.Goto where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Environment

import Simulator.Rules

goto :: Rule
goto (Acqua bb q pus i f) = Acqua bb q (map executeGoto pus) i f
  where
    executeGoto pu =
      case (PU.commands pu,PU.terminator pu) of
        ([], Goto l) -> if PU.tainted pu == False
                            then trace ((show (PU.puId pu)) ++ ": goto") pu'
                            else pu
         where

           PU pId _ _ ce rEnv cEnv ra cc se _ = pu
           BB _ _ c' t' = getBB l bb
           pu' = PU pId c' t' ce rEnv cEnv ra cc se True
        _ -> pu



