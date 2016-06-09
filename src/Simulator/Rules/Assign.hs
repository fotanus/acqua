module Simulator.Rules.Assign where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment
import Simulator.Value as V

import Simulator.Rules.Base

assignV:: Rule
assignV (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignV pus) i f s
  where
    stepAssignV pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((AssignV x v):cs),True) -> trace ((show (PU.puId pu)) ++ ": AssignV " ++ (show x) ++ " " ++ (show v)) pu'
          where
            val = getVal pu v
            puAfterAssign = (setVal pu x val) { PU.commands = cs, locked = True }
            pu' = case val of
                PointerV pt | (V.puId pt) == (PU.puId pu) -> puAfterAssign
                            | otherwise -> error $ "getting stuff form other pu"
                _ -> puAfterAssign
        _ -> pu

assignL:: Rule
assignL (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignL pus) i f s
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
assignI (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignI pus) i f s
  where
    stepAssignI pu =
      case PU.commands pu of
        ((AssignI x v):cs) -> if PU.canExecuteCmds pu
                                then trace ((show (PU.puId pu)) ++ ": AssignI " ++ (show x) ++ " " ++ (show v)) pu'
                              else pu
          where
            pu' = (setVal pu x (NumberV v)) { PU.commands = cs, locked = True}
        _ -> pu

