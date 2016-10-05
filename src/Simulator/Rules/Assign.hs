module Simulator.Rules.Assign where

import qualified Data.Map as Map
import Data.Sequence as Sequence
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value as V
import Simulator.CallRecordSeg as CallRecordSeg
import Simulator.CallRecord

import Simulator.Rules.Base

assignV:: Rule
assignV acqua =
  let (pus',ic') = stepAssignV (processingUnits acqua) (interconnection acqua)
  in acqua { processingUnits = pus', interconnection = ic' }
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
                                crseg = callRecordSeg pu
                                crsegPos = CallRecordSeg.nextFreePos crseg
                                pointer = Pointer (PU.puId pu) crsegPos
                                -- FIXME: this call record created is dummy, used only to reserve the memory position.
                                clos = CallRecord "dummyCR" 0 0 (Sequence.replicate 0 (NumberV 0)) 0
                                crseg' = Map.insert crsegPos (CallRecordV clos) crseg
                                pu'' = (setVal (setVal pu x (PointerV pointer)) v (PointerV pointer)) { PU.commands = cs, callRecordSeg = crseg', enabled = False, locked = True}
                                m = MsgReqClos (PU.puId pu) pointer (V.puId pt) pt
                                i''' = (ConstMsgReqClos m (msgStepsToPropagate acqua)) : i
                _ -> (puAfterAssign,i)
        _ ->
         let
           (pus',i') = stepAssignV pus i
         in
          ((pu:pus'),i')

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

