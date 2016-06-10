module Simulator.Rules.ReceiveUpdateClos where

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

receiveUpdateClos :: Rule
receiveUpdateClos acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      ((ConstMsgUpdateClos (MsgUpdateClos pId pointer idx val)):ms) -> trace ((show (PU.puId pu)) ++ ": receive updateClos")  $ Acqua bb q pus' ms f s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          hp = heap pu

          Just (ClosureV closur) = Map.lookup (addr pointer) hp

          newParams = Seq.update idx val (params closur)
          closur' = closur { params = newParams }

          hp' = Map.insert (addr pointer) (ClosureV closur') hp
          pu' = pu { heap = hp', locked = True }

          pus' = updatePU pus pu'
      _ -> acqua
