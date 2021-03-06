module Simulator.Rules.Messages.UpdatePointer where

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
       ConstMsgUpdatePointer _ 0 -> Just m
       _ -> getNextUpdateMessage ms

updatePointer :: Rule
updatePointer acqua =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgUpdatePointer (MsgUpdatePointer pId pointer idx val) 0) -> trace (("[message "++(show (PU.puId pu))++"]") ++ ": receive updatePointer, now it is " ++ (show callRec')) $ updatePointer (acqua { processingUnits = pus', interconnection = iret })
        where
          Just m' = m
          i = interconnection acqua

          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg

          newParams = Seq.update idx val (params callRec)
          callRec' = callRec { params = newParams }

          crseg' = Map.insert (addr pointer) (CallRecordV callRec') crseg
          (iret, pu') = if (lockedMsg pu)
                      then (i'', pu)
                      else (i',  pu { callRecordSeg = crseg', lockedMsg = True })

          i' = delete m' i
          i'' = i' ++ [(ConstMsgUpdatePointer (MsgUpdatePointer pId pointer idx val) 1)]
          pus' = updatePU pus pu'
      _ -> acqua
