module Simulator.Rules.ReceiveResponse where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

receiveResponse :: Rule
receiveResponse acqua =
  case (interconnection acqua) of
      ((ConstMsgResponse (MsgResponse pId envId x v)):ms) -> trace ((show (PU.puId pu)) ++ ": receive response")  $ acqua { processingUnits = pus', interconnection = ms, finishFlag = f' }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          env = environments pu
          cc = callCount pu
          Just cenv  = Map.lookup envId env
          cenv' = Map.insert x v cenv
          env' = Map.insert envId cenv' env
          Just nCalls = Map.lookup envId cc
          cc' = Map.insert envId (nCalls-1) cc
          pu' = pu { environments = env', callCount = cc', locked = True }
          pus' = updatePU pus pu'
          f' = if pId == 0 && (nCalls-1) == 0
                 then True
                 else False
      _ -> acqua
