module Simulator.Rules.If where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value

import Simulator.Rules.Base

ifRule :: Rule
ifRule (Acqua bb q pus i f s) = Acqua bb q (map executeIf pus) i f s
  where
    executeIf pu =
      case (PU.commands pu,PU.terminator pu, PU.canExecuteCmds pu) of
        ([], If x l, True) -> trace ((show (PU.puId pu))++": " ++ (show (currentEnv pu)) ++ "if " ++ x ++ " goto " ++ l ++ "( x = " ++ (show val) ++ ")") pu'
         where
           ce = currentEnv pu
           env = environments pu
           Just cenv = Map.lookup ce env
           Just (NumberV val) = Map.lookup x cenv
           BB _ _ c' t'  = if val > 0
                           then lookupBB bb l
                           else lookupBB bb (dummy l)

           pu' = pu {PU.commands = c', PU.terminator = t', locked = True }
        _ -> pu


dummy :: Label -> Label
dummy l = "dummy" ++ (drop 4 l)

