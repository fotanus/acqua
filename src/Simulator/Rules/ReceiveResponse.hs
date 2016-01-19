module Simulator.Rules.ReceiveResponse where

import Data.List
import qualified Data.Map as Map
import Debug.Trace

import AcquaIR.Language as IR
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules

receiveResponse :: Rule
receiveResponse acqua  =
  let
    Acqua bb q pus i f = acqua
  in case i of
      [] -> acqua
      (m:ms) -> trace "receive response " $ Acqua bb q pus' ms f
        where
          getMessage inter = case inter of
            [] -> Nothing
            (m:ms) -> Just (m,ms)

          -- pu
          Message pId envId x v = m
          Just pu = Data.List.find (\pu -> (PU.puId pu) == pId) pus
          PU _ c t ce rEnv cEnv ra cc se = pu

          Just cenv  = Map.lookup envId rEnv
          cenv' = Map.insert x v cenv
          rEnv' = Map.insert envId cenv' rEnv
          Just nCalls = Map.lookup envId cc
          cc' = Map.insert envId (nCalls-1) cc
          pu' = traceShowId $ PU pId c t ce rEnv' cEnv ra cc' se
          pus' = updatePU pus pu'
