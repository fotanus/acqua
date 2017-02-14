module Simulator.Rules.Assign where

import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value as V

import Simulator.Rules.Base
assignV:: Rule
assignV acqua =
    acqua { processingUnits = map stepAssignV (processingUnits acqua) }
  where
    stepAssignV pu =
      case PU.commands pu of
        ((AssignV x v):cs) -> if PU.canExecuteCmds pu
                              then trace ((show (PU.puId pu)) ++ ": AssignV" ++ (show x) ++ " " ++ (show v)) pu'
                              else pu
          where
            val = getVal pu v
            pu' = (setVal pu x val) { PU.commands = cs, locked = True}
        _ -> pu

assignL:: Rule
assignL acqua =
    acqua { processingUnits = map stepAssignL (processingUnits acqua) }
  where
    stepAssignL pu =
      case PU.commands pu of
        ((AssignL x v):cs) -> if PU.canExecuteCmds pu
                              then trace ((show (PU.puId pu)) ++ ": AssignL" ++ (show x) ++ " " ++ (show v)) pu'
                              else pu
          where
            pu' = (setVal pu x (LabelV v)) { PU.commands = cs, locked = True}
        _ -> pu

assignI:: Rule
assignI acqua =
    acqua { processingUnits = map stepAssignI (processingUnits acqua) }
  where
    stepAssignI pu =
      case PU.commands pu of
        ((AssignI x v):cs) -> if PU.canExecuteCmds pu
                                then trace ((show (PU.puId pu)) ++ ": AssignI " ++ (show x) ++ " " ++ (show v)) pu'
                              else pu
          where
            pu' = (setVal pu x (NumberV v)) { PU.commands = cs, locked = True}
        _ -> pu

