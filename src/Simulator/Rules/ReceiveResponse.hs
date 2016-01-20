module Simulator.Rules.ReceiveResponse where

import Data.List
import qualified Data.Map as Map
import Debug.Trace

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

receiveResponse :: Rule
receiveResponse acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      [] -> acqua
      (m:ms) -> trace ((show (PU.puId pu)) ++ ": receive response")  $ Acqua bb q pus' ms f' s
        where
          Message pId envId x v = m
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          PU _ c t ce rEnv cEnv ra cc se _ = pu
          Just cenv  = Map.lookup envId rEnv
          cenv' = Map.insert x v cenv
          rEnv' = Map.insert envId cenv' rEnv
          Just nCalls = Map.lookup envId cc
          cc' = Map.insert envId (nCalls-1) cc
          pu' = PU pId c t ce rEnv' cEnv ra cc' se True
          pus' = updatePU pus pu'
          f' = if pId == 0
                 then True
                 else False
