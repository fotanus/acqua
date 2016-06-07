module Simulator.Rules.ReceiveUpdate where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Heap
import Simulator.Value
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
          env = environments pu
          hp = heap pu

          Just cenv  = Map.lookup envId env
          Just (PointerV pointer) = Map.lookup "closure" cenv
          Just (ClosureV closur) = Map.lookup (addr pointer) hp

          newParams = Seq.update idx val (params closur)
          closur' = closur { params = newParams }

          hp' = Map.insert (addr pointer) (ClosureV closur') hp
          pu' = pu { heap = hp', locked = True }

          pus' = updatePU pus pu'
          f' = if pId == 0
                 then True
                 else False
      _ -> acqua
