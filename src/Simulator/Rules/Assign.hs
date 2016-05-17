module Simulator.Rules.Assign where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules.Base

assignV:: Rule
assignV (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignV pus) i f s
  where
    stepAssignV pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((AssignV x v):cs),True) -> trace ((show (PU.puId pu)) ++ ": AssignV " ++ (show x) ++ " " ++ (show v)) pu'
          where
            PU pId _ t ce env ra cc se omq enbl _ = pu
            Just cenv = Map.lookup ce env
            Just val = Map.lookup v cenv
            cenv' = case val of
                    BaseValV (LabelV v') -> Map.insert x (BaseValV (LabelV v')) cenv
                    BaseValV (NumberV v') -> Map.insert x (BaseValV (NumberV v')) cenv
                    _ -> error $ "closures not implemented"
            env' = Map.insert ce cenv' env
            pu' = PU pId cs t ce env' ra cc se omq enbl True
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
            PU pId _ t ce env ra cc se omq enbl _ = pu
            Just cenv = Map.lookup ce env
            cenv' = Map.insert x (BaseValV (LabelV v)) cenv
            env' = Map.insert ce cenv' env
            pu' = PU pId cs t ce env' ra cc se omq enbl True
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
            PU pId _ t ce env ra cc se omq enbl _ = pu
            Just cenv = Map.lookup ce env
            cenv' = Map.insert x (BaseValV (NumberV v)) cenv
            env' = Map.insert ce cenv' env
            pu' = PU pId cs t ce env' ra cc se omq enbl True
        _ -> pu

