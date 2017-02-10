module Simulator.Rules.OuterCopy where

import qualified Data.Map as Map
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value as V
import Simulator.CallRecordSeg as CallRecordSeg
import Simulator.CallRecord

import Simulator.Rules.Base

outerCopy:: Rule
outerCopy acqua =
  let (pus',ic') = stepOuterCopy (processingUnits acqua) (interconnection acqua)
  in acqua { processingUnits = pus', interconnection = ic' }
  where
    stepOuterCopy [] i = ([],i)
    stepOuterCopy (pu:pus) i =
      case (PU.commands pu,PU.canExecuteCmds pu) of
        (((OuterCopy x1 x2):cs),True) -> trace ((show (PU.puId pu)) ++ ": " ++ x1 ++ " = OuterCopy "  ++ x2) ((pu':pus'),i'')
          where

            pId = PU.puId pu
            PointerV pointer = getVal pu x2

            -- create dummy CR to hold the space
            crseg = PU.callRecordSeg pu
            crsegPos = CallRecordSeg.nextFreePos crseg
            dummyCr = emptyCallRecord { functionName = "OuterCopyCR" }
            crseg' = Map.insert crsegPos (CallRecordV dummyCr) crseg

            -- send message to copy
            newPointer = Pointer pId crsegPos
            m = MsgReqPointer pId newPointer (V.puId pointer) pointer
            i' = (ConstMsgReqPointer m (msgStepsToPropagate acqua)) : i

            pu' = (setVal pu x1 (PointerV newPointer)) { PU.commands = cs, callRecordSeg = crseg', enabled = False, locked = True}
            (pus',i'') = (stepOuterCopy pus i')
        _ -> let (pus',i') = stepOuterCopy pus i
             in (pu:pus',i')
