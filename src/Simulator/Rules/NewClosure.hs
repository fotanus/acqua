module Simulator.Rules.NewClosure where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.Heap
import Simulator.Closure

import Simulator.Rules.Base

newClosure :: Rule
newClosure (Acqua bb q pus i f s) =
    Acqua bb q (map stepNewClosure pus) i f s
  where
    stepNewClosure pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((NewClosure x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": NewClosure " ++ (show x) ++ " " ++ (show n)) pu'
          where
            pId = PU.puId pu
            ce = currentEnv pu
            env = environments pu
            hp = heap pu
            Just cenv = Map.lookup ce env

            hpPos = Map.size hp
            pointer = Pointer pId hpPos
            clos = Closure "" 0 0 (Sequence.replicate n (NumberV 0))
            cenv' = Map.insert x (PointerV pointer) cenv
            env' = Map.insert ce cenv' env
            hp' = Map.insert hpPos (ClosureV clos) hp
            pu' = pu { PU.commands = cs, environments = env', heap = hp', locked = True }
        _ -> pu
