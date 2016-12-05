module Simulator.Rules.CallL where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.Job
import Simulator.ProcessingUnit as PU
import Simulator.Value
import Simulator.CallRecordSeg
import Simulator.CallRecord
import Simulator.ReturnAddrVar

import Simulator.Rules.Base

callL :: Rule
callL acqua =
    acqua { processingUnits = stepCallL (processingUnits acqua) }

stepCallL :: [ProcessingUnit] -> [ProcessingUnit]
stepCallL [] = []
stepCallL (pu:pus) =
  case (PU.commands pu, PU.canExecuteCmds pu) of
    ((CallL x1 idxname x2):cs,True) -> trace ((show (PU.puId pu)) ++  ": callL" ) (pu':pus')
      where
        pus' = stepCallL pus

        -- set job on outgoing job queue
        ce = PU.currentEnv pu
        pId = PU.puId pu
        envs = PU.environments pu
        crseg = PU.callRecordSeg pu
        Just cenv = Map.lookup ce envs
        Just (PointerV retPointer) = Map.lookup x1 cenv
        Just (PointerV pointer) = Map.lookup x2 cenv
        Just (NumberV idx) = Map.lookup idxname cenv
        Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg
        j = Job pId (CallSource pointer) (Seq.length (params callRec)) ce (ListVal retPointer idx) False
        ojq = [j] ++ (outgoingJobQueue pu)

        -- increment calls count
        cc  = PU.callCount pu
        Just nCalls = Map.lookup ce cc
        cc' = Map.insert ce (nCalls+1) cc
        ocr = Map.insert ce pointer (originCallRec pu)
        pu' = pu { PU.commands = cs, PU.callCount = cc', originCallRec = ocr, outgoingJobQueue = ojq, PU.locked = True }

    _ -> pu:(stepCallL pus)
