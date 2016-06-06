module Simulator.Rules.ReceiveUpdate where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Environment
import Simulator.Closure

import Simulator.Rules.Base

receiveUpdate :: Rule
receiveUpdate acqua  =
  let
    Acqua bb q pus i _ s = acqua
  in case i of
      ((ConstMsgUpdate (MsgUpdate pId envId idx val)):ms) -> trace ((show (PU.puId pu)) ++ ": receive update")  $ Acqua bb q pus' ms f' s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          PU _ c t ce env ra cc se omq enbl _ = pu
          Just cenv  = Map.lookup envId env
          Just (ClosureV closure) = Map.lookup "closure" cenv
          newParams = Seq.update idx val (params closure)
          closure' = closure { params = newParams }
          cenv' = Map.insert "closure" (ClosureV closure') cenv
          env' = Map.insert envId cenv' env
          pu' = PU pId c t ce env' ra cc se omq enbl True
          pus' = updatePU pus pu'
          f' = if pId == 0
                 then True
                 else False
      _ -> acqua
