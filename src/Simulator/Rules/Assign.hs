module Simulator.Rules.Assign where

import qualified Data.Map as Map
import Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value as V
import Simulator.Heap as Heap
import Simulator.Closure

import Simulator.Rules.Base

assignV:: Rule
assignV (Acqua bb q processingunits ic f s) =
  let (pus',ic') = stepAssignV processingunits ic
  in Acqua bb q pus' ic' f s
  where
    stepAssignV [] i = ([],i)
    stepAssignV (pu:pus) i =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((AssignV x v):cs),True) -> trace ((show (PU.puId pu)) ++ ": AssignV " ++ (show x) ++ " " ++ (show v)) ((pu':pus'),i'')
          where
            val = getVal pu v
            puAfterAssign = (setVal pu x val) { PU.commands = cs, locked = True }
            (pus',i'') = (stepAssignV pus i')
            (pu',i') = case val of
                PointerV pt | (V.puId pt) == (PU.puId pu) -> (puAfterAssign,i)
                            | otherwise -> traceShow ("AssignV from other PU, starting to copy " ++ (show pt)) (pu'',i''')
                              where
                                hp = heap pu
                                hpPos = Heap.nextFreePos hp
                                pointer = Pointer (PU.puId pu) hpPos
                                clos = Closure "" 0 0 (Sequence.replicate 5 (NumberV 0))
                                hp' = Map.insert hpPos (ClosureV clos) hp
                                pu'' = (setVal pu x (PointerV pointer)) { PU.commands = cs, heap = hp', enabled = False, locked = True}
                                m = MsgReqClos (PU.puId pu) pointer (V.puId pt) pt
                                i''' = (ConstMsgReqClos m) : i
                _ -> (puAfterAssign,i)
        _ ->
         let
           (pus',i') = stepAssignV pus i
         in
          ((pu:pus'),i')

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

