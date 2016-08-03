module Simulator.Rules.ReceiveResponse where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

getNextUpdateMessage :: Interconnection -> Maybe Message
getNextUpdateMessage [] = Nothing
getNextUpdateMessage (m:ms) =
   case m of
       ConstMsgResponse _ 0 -> Just m
       _ -> getNextUpdateMessage ms

receiveResponse :: Rule
receiveResponse acqua =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgResponse (MsgResponse pId envId x v) 0) -> trace ((show (PU.puId pu)) ++ ": receive response")  $ receiveResponse (acqua { processingUnits = pus', interconnection = iret, finishFlag = f' })
        where
          pus = processingUnits acqua
          i = interconnection acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          env = environments pu
          cc = callCount pu
          Just cenv  = Map.lookup envId env
          cenv' = Map.insert x v cenv
          env' = Map.insert envId cenv' env
          Just nCalls = Map.lookup envId cc
          cc' = Map.insert envId (nCalls-1) cc
          (iret, pu') = if (lockedMsg pu)
                      then (i'', pu)
                      else (i', pu { environments = env', callCount = cc', lockedMsg = True})
          Just m' = m
          i' = delete m' i
          i'' = (ConstMsgResponse (MsgResponse pId envId x v) 1):i'
          pus' = updatePU pus pu'
          f' = if pId == 0 && (nCalls-1) == 0
                 then True
                 else False
      _ -> acqua
