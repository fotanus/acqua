module Simulator.Rules.NewClosure where

import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

newClosure :: Rule
newClosure (Acqua bb q pus i f s) =
    Acqua bb q (map stepNewClosure pus) i f s
  where
    stepNewClosure pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((NewClosure x n):cs),True) -> trace ((show (PU.puId pu)) ++ ": NewClosure " ++ (show x) ++ " " ++ (show n)) pu'
          where
            PU pId _ t ce env ra cc se omq enbl _ = pu
            Just cenv = Map.lookup ce env

            clos = Closure "" 0 0 (Sequence.fromList [])
            cenv' = Map.insert x (ClosureV clos) cenv
            env' = Map.insert ce cenv' env
            pu' = PU pId cs t ce env' ra cc se omq enbl True
        _ -> pu
