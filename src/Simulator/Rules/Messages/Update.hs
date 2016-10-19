module Simulator.Rules.Messages.Update where

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

getNextUpdateMessage :: Interconnection -> Maybe Message
getNextUpdateMessage [] = Nothing
getNextUpdateMessage (m:ms) =
   case m of
       ConstMsgUpdate  _ 0 -> Just m
       _ -> getNextUpdateMessage ms


update :: Rule
update acqua  =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgUpdate (MsgUpdate pId pointerS idx val) 0) -> trace ((show (PU.puId pu)) ++ ": receive update")  $ update (acqua { processingUnits = pus', interconnection = iret, finishFlag = f' })
        where
          Just m' = m
          i = interconnection acqua
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu
          Just (CallRecordV callRec) = Map.lookup (addr pointerS) crseg
          -- FIXME: Avoid rewriting of value set when creating the job
          newParams = if Seq.index (params callRec) idx == (NumberV 0)
                      then Seq.update idx val (params callRec)
                      else params callRec
          callRec' = callRec { params = newParams }

          crseg' = Map.insert (addr pointerS) (CallRecordV callRec') crseg

          (iret, pu') = if (lockedMsg pu)
                      then (i'', pu)
                      else (i',  pu { callRecordSeg = crseg', lockedMsg = True })
          i' = delete m' i
          i'' = i' ++ [(ConstMsgUpdate (MsgUpdate pId pointerS idx val) 1)]
          pus' = updatePU pus pu'
          f' = if pId == 0
                 then True
                 else False
      _ -> acqua
