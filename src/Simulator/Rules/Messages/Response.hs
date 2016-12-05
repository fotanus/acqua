module Simulator.Rules.Messages.Response where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.ReturnAddrVar
import Simulator.CallRecordSeg
import Simulator.CallRecord
import Simulator.List
import Simulator.Value

import Simulator.Rules.Base

getNextUpdateMessage :: Interconnection -> Maybe Message
getNextUpdateMessage [] = Nothing
getNextUpdateMessage (m:ms) =
   case m of
       ConstMsgResponse _ 0 -> Just m
       _ -> getNextUpdateMessage ms

response :: Rule
response acqua =
  let m = getNextUpdateMessage (interconnection acqua)
  in case m of
      Just (ConstMsgResponse (MsgResponse pId envId retval v isMapF) 0) ->
          trace ((show (PU.puId pu)) ++ ": receive response")  $ response (acqua { processingUnits = pus', interconnection = iret, finishFlag = f' })
        where
          pus = processingUnits acqua
          i = interconnection acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          env = environments pu
          cc = callCount pu
          Just cenv  = Map.lookup envId env

          cenv' = case retval of
                    EnvVal x -> Map.insert x v cenv
                    _ -> cenv

          crseg = callRecordSeg pu
          crseg' = case retval of
                     ListVal pointer idx -> Map.insert (addr origptr) cr' $ Map.insert (addr pointer) list' crseg
                        where
                            Just (ListV (List lsize list)) = Map.lookup (addr pointer) crseg
                            list' = ListV (List lsize (listSetPos list idx v))

                            -- set max time to dealocate when call count reaches zero
                            Just origptr = Map.lookup envId $ originCallRec pu
                            Just (CallRecordV cr) = Map.lookup (addr origptr) crseg
                            cr' = if isMapF && nCalls-1 == 0
                                  then CallRecordV $ cr { timeout = maxTimeout }
                                  else CallRecordV $ cr { timeout = maxTimeout + 1 }
                     _ -> crseg

          env' = Map.insert envId cenv' env
          Just nCalls = Map.lookup envId cc
          cc' = Map.insert envId (nCalls-1) cc


          (iret, pu') = if (lockedMsg pu)
                      then (i'', pu)
                      else (i', pu { environments = env', callCount = cc', callRecordSeg = crseg', lockedMsg = True})
          Just m' = m
          i' = delete m' i
          i'' = i' ++ [(ConstMsgResponse (MsgResponse pId envId retval v isMapF) 1)]
          pus' = updatePU pus pu'
          f' = if pId == 0 && (nCalls-1) == 0
                 then True
                 else False
      _ -> acqua
