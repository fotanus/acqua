module Simulator.Rules.If where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

ifRule :: Rule
ifRule (Acqua bb q pus i f s) = Acqua bb q (map executeIf pus) i f s
  where
    executeIf pu =
      case (PU.commands pu,PU.terminator pu, PU.canExecuteCmds pu) of
        ([], If x l, True) -> trace ((show (PU.puId pu))++": if " ++ x ++ " goto " ++ l ++ "( x = " ++ (show val) ++ ")") pu'
         where

           PU pId _ _ ce env ra cc se omq enbl _ = pu
           Just cenv = Map.lookup ce env
           Just (BaseValV (NumberV val)) = Map.lookup x cenv
           BB _ _ c' t'  = if val > 0
                           then getBB l bb
                           else getBB (dummy l) bb

           pu' = PU pId c' t' ce env ra cc se omq enbl True
        _ -> pu


dummy :: Label -> Label
dummy l = "dummy" ++ (drop 4 l)

