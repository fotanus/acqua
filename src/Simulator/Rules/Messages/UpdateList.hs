module Simulator.Rules.Messages.UpdateList where

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
       ConstMsgUpdateList  _ 0 -> Just m
       _ -> getNextUpdateMessage ms

updateList :: Rule
updateList acqua =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgUpdateList (MsgUpdateList pId pointer val) 0) -> trace ((show (PU.puId pu)) ++ ": receive updateList, now it is " ++ (show list')) $ updateList (acqua { processingUnits = pus', interconnection = iret })
        where
          Just m' = m
          i = interconnection acqua

          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          Just (ListV (List listSize items)) = Map.lookup (addr pointer) crseg

          list' = List listSize (items++[val])

          crseg' = Map.insert (addr pointer) (ListV list') crseg
          (iret, pu') = if (lockedMsg pu)
                      then (i'', pu)
                      else (i',  pu { callRecordSeg = crseg', lockedMsg = True })

          i' = delete m' i
          i'' = i' ++ [(ConstMsgUpdateList (MsgUpdateList pId pointer val) 1)]
          pus' = updatePU pus pu'
      _ -> acqua
