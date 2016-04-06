module Simulator.Rules.ReceiveUpdate where

import Data.List
import qualified Data.Map as Map
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

receiveUpdate :: Rule
receiveUpdate acqua  =
  let
    Acqua bb q pus i _ s = acqua
  in case i of
      ((ConstMsgUpdate (MsgUpdate pId envId x v)):ms) -> trace ((show (PU.puId pu)) ++ ": receive update")  $ Acqua bb q pus' ms f' s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          PU _ c t ce rEnv cEnv ra cc se omq enbl _ = pu
          Just cenv  = Map.lookup envId rEnv
          cenv' = Map.insert x v cenv
          rEnv' = Map.insert envId cenv' rEnv
          pu' = PU pId c t ce rEnv' cEnv ra cc se omq enbl True
          pus' = updatePU pus pu'
          f' = if pId == 0
                 then True
                 else False
      _ -> acqua