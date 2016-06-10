module Simulator.Rules.ReceiveUpdateMetaClos where

import Data.List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Heap
import Simulator.Value
import Simulator.Closure as Closure

import Simulator.Rules.Base

receiveUpdateMetaClos :: Rule
receiveUpdateMetaClos acqua  =
  let
    Acqua bb q pus i f s = acqua
  in case i of
      ((ConstMsgUpdateMetaClos (MsgUpdateMetaClos pId pointer fnName count missing)):ms) -> trace ((show (PU.puId pu)) ++ ": receive updateMetaClos")  $ Acqua bb q pus' ms f s
        where
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          hp = heap pu

          Just (ClosureV closur) = Map.lookup (addr pointer) hp

          closur' = closur { functionName = fnName, Closure.paramCount = count, Closure.paramMissing = missing }

          hp' = Map.insert (addr pointer) (ClosureV closur') hp
          pu' = pu { heap = hp', locked = True }

          pus' = updatePU pus pu'
      _ -> acqua
