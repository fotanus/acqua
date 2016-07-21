module Simulator.Rules.ReceiveUpdate where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.CallRecord

import Simulator.Rules.Base

receiveUpdate :: Rule
receiveUpdate acqua  =
  case (interconnection acqua) of
      ((ConstMsgUpdate (MsgUpdate pId envId idx val) 0):ms) -> trace ((show (PU.puId pu)) ++ ": receive update")  $ acqua { processingUnits = pus', interconnection = ms, finishFlag = f' }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          env = environments pu
          crseg = callRecordSeg pu

          Just cenv  = Map.lookup envId env
          Just (PointerV pointer) = Map.lookup "callRecord" cenv
          Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg

          newParams = Seq.update idx val (params callRec)
          callRec' = callRec { params = newParams }

          crseg' = Map.insert (addr pointer) (CallRecordV callRec') crseg
          pu' = pu { callRecordSeg = crseg', locked = True }

          pus' = updatePU pus pu'
          f' = if pId == 0
                 then True
                 else False
      _ -> acqua
