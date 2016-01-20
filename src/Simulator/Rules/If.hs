module Simulator.Rules.If where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules

ifRule :: Rule
ifRule (Acqua bb q pus i f s) = Acqua bb q (map executeIf pus) i f s
  where
    executeIf pu =
      case (PU.commands pu,PU.terminator pu) of
        ([], If x l) -> if PU.tainted pu == False
                            then trace (": if") pu'
                            else pu
         where

           PU pId _ _ ce rEnv cEnv ra cc se _ = pu
           Just cenv = Map.lookup ce rEnv
           Just (NumberValue val) = Map.lookup x cenv
           BB _ _ c' t'  = if val > 0
                           then getBB l bb
                           else getBB (dummy l) bb

           pu' = PU pId c' t' ce rEnv cEnv ra cc se True
        _ -> pu


dummy :: Label -> Label
dummy l = "dummy" ++ (drop 4 l)

