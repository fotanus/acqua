module Simulator.Rules.InnerCopy where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.CallRecordSeg as CallRecordSeg
import Simulator.CallRecord as CR
import Simulator.List as L

import Simulator.Rules.Base

innerCopy :: Rule
innerCopy acqua =
    acqua { processingUnits = stepInnerCopy (processingUnits acqua) }

stepInnerCopy :: [ProcessingUnit] -> [ProcessingUnit]
stepInnerCopy [] = []
stepInnerCopy (pu:pus) =
  case (PU.commands pu, PU.canExecuteCmds pu) of
    ((InnerCopy x1 x2):cs,True) -> trace ((show (PU.puId pu)) ++  ": innerCopy" ) (pu':pus')
      where
        pus' = stepInnerCopy pus

        ce = PU.currentEnv pu
        pId = PU.puId pu
        envs = PU.environments pu
        crseg = PU.callRecordSeg pu
        Just cenv = Map.lookup ce envs

        -- copy callrec on crseg
        Just (PointerV pointer) = Map.lookup x2 cenv
        Just valueBeingCopied  = traceShowId $ Map.lookup (addr pointer) crseg
        crsegPos = CallRecordSeg.nextFreePos crseg
        crseg' = Map.insert crsegPos valueBeingCopied crseg
        stallC = case valueBeingCopied of
                 CallRecordV crCopy -> (Seq.length (CR.params crCopy)) + 3
                 ListV crCopy -> (length (L.params crCopy))
                 _ -> error $ "InnerCoping something that is not a list or call record: " ++ (show valueBeingCopied)

        -- add pointer to new copy
        newPointer = Pointer (PU.puId pu) crsegPos
        pu' =  (setVal pu x1 (PointerV newPointer)) { PU.commands = cs, callRecordSeg = crseg', PU.locked = True, PU.stallCycles = stallC }

    _ -> pu:(stepInnerCopy pus)
