module Simulator.Rules.Op where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

op :: Rule
op (Acqua bb q pus i f s) =
    Acqua bb q (map stepOp pus) i f s
  where
    stepOp pu =
      case (PU.commands pu, PU.canExecuteCmds pu) of
        ((Op x1 opc x2):cs, True) -> trace ((show (PU.puId pu)) ++ ": OP") pu'''
          where
            PU pId _ t ce env ra cc se omq enbl _ = pu
            Just cenv = Map.lookup ce env
            Just (BaseValV (NumberV v1)) = Map.lookup x1 cenv
            Just (BaseValV (NumberV v2)) = Map.lookup x2 cenv
            val = case opc of
              And -> v1 + v2
              Or -> v1 + v2
              Add -> v1 + v2
              Sub -> v1 - v2
              Mult -> v1 * v2
              Equal -> v1 - v2
              NotEqual -> v1 - v2
              Greater -> v1 - v2
              GreaterEqual -> v1 + 1 - v2
              Lesser -> v2 - v1
              LesserEqual -> v2 + 1 - v1
            cenv' = Map.insert "resp" (BaseValV (NumberV val)) cenv
            pu''' = PU pId cs t ce env ra cc se omq enbl True
        _ -> pu

