module Simulator.Rules.Assign where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment
import Simulator.Value

import Simulator.Rules.Base

assignV:: Rule
assignV (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignV pus) i f s
  where
    stepAssignV pu =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((AssignV x v):cs),True) -> trace ((show (PU.puId pu)) ++ ": AssignV " ++ (show x) ++ " " ++ (show v)) pu'
          where
            ce = currentEnv pu
            env = environments pu
            Just cenv = Map.lookup ce env
            Just val = Map.lookup v cenv
            cenv' = Map.insert x val cenv
            env' = Map.insert ce cenv' env
            pu' = pu { currentEnv = env', locked = True }
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
            ce = currentEnv pu
            env = environments pu
            Just cenv = Map.lookup ce env
            cenv' = Map.insert x (LabelV v) cenv
            env' = Map.insert ce cenv' env
            pu' = pu { environments = env', locked = True}
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
            ce = currentEnv pu
            env = environments pu
            Just cenv = Map.lookup ce env
            cenv' = Map.insert x (NumberV v) cenv
            env' = Map.insert ce cenv' env
            pu' = pu { environments = env', locked = True}
        _ -> pu

