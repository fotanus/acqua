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
      case (PU.commands pu,PU.locked pu) of
        (((AssignV x v):cs),False) -> trace ((show (PU.puId pu)) ++ ": AssignV " ++ (show x) ++ " " ++ (show v)) pu'
          where
            PU pId _ t ce rEnv cEnv ra cc se _ enbl = pu
            Just cenv = Map.lookup ce rEnv
            Just val = Map.lookup v cenv
            cenv' = case val of
                    LabelValue v' -> Map.insert x (LabelValue v') cenv
                    NumberValue v' -> Map.insert x (NumberValue v') cenv
            rEnv' = Map.insert ce cenv' rEnv
            pu' = PU pId cs t ce rEnv' cEnv ra cc se True enbl
        _ -> pu

assignL:: Rule
assignL (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignL pus) i f s
  where
    stepAssignL pu =
      case PU.commands pu of
        ((AssignL x v):cs) -> if PU.locked pu == False
                              then trace ((show (PU.puId pu)) ++ ": AssignL" ++ (show x) ++ " " ++ (show v)) pu'
                              else pu
          where
            PU pId _ t ce rEnv cEnv ra cc se _ enbl = pu
            Just cenv = Map.lookup ce rEnv
            cenv' = Map.insert x (LabelValue v) cenv
            rEnv' = Map.insert ce cenv' rEnv
            pu' = PU pId cs t ce rEnv' cEnv ra cc se True enbl
        _ -> pu

assignI:: Rule
assignI (Acqua bb q pus i f s) =
    Acqua bb q (map stepAssignI pus) i f s
  where
    stepAssignI pu =
      case PU.commands pu of
        ((AssignI x v):cs) -> if PU.locked pu == False
                                then trace ((show (PU.puId pu)) ++ ": AssignI" ++ (show x) ++ " " ++ (show v)) pu'
                              else pu
          where
            PU pId _ t ce rEnv cEnv ra cc se _ enbl = pu
            Just cenv = Map.lookup ce rEnv
            cenv' = Map.insert x (NumberValue v) cenv
            rEnv' = Map.insert ce cenv' rEnv
            pu' = PU pId cs t ce rEnv' cEnv ra cc se True enbl
        _ -> pu

