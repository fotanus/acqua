module Simulator.Rules.ReceiveResponse where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

receiveResponse :: Rule
receiveResponse acqua  =
  let
    Acqua bb q pus i _ s = acqua
  in case i of
      ((ConstMsgResponse (MsgResponse pId envId x v)):ms) -> trace ((show (PU.puId pu)) ++ ": receive response")  $ Acqua bb q pus' ms f' s
        where
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
          f' = if pId == 0
                 then True
                 else False
      _ -> acqua
