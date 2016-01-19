module Simulator.Rules.Assign where

import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Environment

import Simulator.Rules

assignV:: Rule
assignV (Acqua bb q pus i f) = 
    Acqua bb q (map stepAssignV pus) i f
  where
    stepAssignV pu =
      case PU.commands pu of
        ((AssignV x v):cs) -> trace "AssignV" pu'
          where
            PU pId _ t ce rEnv cEnv ra cc se = pu
            Just cenv = Map.lookup ce rEnv
            Just val = Map.lookup v cenv
            cenv' = case val of
                    LabelValue v' -> Map.insert x (LabelValue v') cenv
                    NumberValue v' -> Map.insert x (NumberValue v') cenv
            rEnv' = Map.insert ce cenv' rEnv
            pu' = PU pId cs t ce rEnv' cEnv ra cc se
        _ -> pu

assignL:: Rule
assignL (Acqua bb q pus i f) = 
    Acqua bb q (map stepAssignL pus) i f
  where
    stepAssignL pu =
      case PU.commands pu of
        ((AssignL x v):cs) -> trace "AssignL" pu'
          where
            PU pId _ t ce rEnv cEnv ra cc se = pu
            Just cenv = Map.lookup ce rEnv
            cenv' = Map.insert x (LabelValue v) cenv
            rEnv' = Map.insert ce cenv' rEnv
            pu' = PU pId cs t ce rEnv' cEnv ra cc se
        _ -> pu

assignI:: Rule
assignI (Acqua bb q pus i f) = 
    Acqua bb q (map stepAssignI pus) i f
  where
    stepAssignI pu =
      case PU.commands pu of
        ((AssignI x v):cs) -> trace "AssignI" pu'
          where
            PU pId _ t ce rEnv cEnv ra cc se = pu
            Just cenv = Map.lookup ce rEnv
            cenv' = Map.insert x (NumberValue v) cenv
            rEnv' = Map.insert ce cenv' rEnv
            pu' = PU pId cs t ce rEnv' cEnv ra cc se
        _ -> pu

  
  
