module Simulator.Rules.ReceiveUpdateMetaList where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.CallRecordSeg
import Simulator.Value
import Simulator.List

import Simulator.Rules.Base

getNextUpdateMessage :: Interconnection -> Maybe Message
getNextUpdateMessage [] = Nothing
getNextUpdateMessage (m:ms) =
   case m of
       ConstMsgUpdateMetaList  _ 0 -> Just m
       _ -> getNextUpdateMessage ms


receiveUpdateMetaList :: Rule
receiveUpdateMetaList acqua  =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgUpdateMetaList (MsgUpdateMetaList pId pointer listSize) 0) -> trace ((show (PU.puId pu)) ++ ": receive updateMetaList") $ receiveUpdateMetaList $ acqua { processingUnits = pus', interconnection = iret}
        where
          Just m' = m
          i = interconnection acqua
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          list = List listSize []
          crseg' = Map.insert (addr pointer) (ListV list) crseg

          (iret, pu') = if (lockedMsg pu)
                      then (i'', pu)
                      else (i',  pu { callRecordSeg = crseg', lockedMsg = True })
          i' = delete m' i
          i'' = (ConstMsgUpdateMetaList (MsgUpdateMetaList pId pointer listSize) 1):i'

          pus' = updatePU pus pu'
      _ -> acqua
