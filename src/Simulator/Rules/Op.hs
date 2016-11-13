module Simulator.Rules.Op where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value

import Simulator.Rules.Base

op :: Rule
op acqua = acqua { processingUnits = newPus }
  where
    newPus = map stepOp (processingUnits acqua)
    stepOp pu =
      case (PU.commands pu, PU.canExecuteCmds pu) of
        ((Op res x1 opc x2):cs, True) -> trace ((show (PU.puId pu)) ++ ": OP with result " ++ (show val)) pu''
          where
            NumberV v1 = getVal pu x1
            NumberV v2 = getVal pu x2
            val = case opc of
              And -> if v1 > 0 && v2 > 0 then 1 else 0
              Or -> if v1 > 0 ||  v2 > 0 then 1 else 0
              Add -> v1 + v2
              Sub -> v1 - v2
              Mult -> v1 * v2
              Div -> v1 `div` v2
              Equal -> if v1 == v2 then 1 else 0
              NotEqual -> if v1 == v2 then 0 else 1
              Greater -> if v1 > v2 then 1 else 0
              GreaterEqual -> if v1 >= v2 then 1 else 0
              Lesser -> if v1 < v2 then 1 else 0
              LesserEqual -> if v1 <= v2 then 1 else 0
            pu'' = (setVal pu res (NumberV val)) { PU.commands = cs, locked = True, PU.stallCycles = case opc of Mult -> 3 ; _ -> 0 }
        _ -> pu

