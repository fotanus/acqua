module Simulator.Rules.ReceiveUpdateMetaClos where

import Data.List
import qualified Data.Map as Map
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
  case (interconnection acqua) of
      ((ConstMsgUpdateMetaClos (MsgUpdateMetaClos pId pointer fnN count missing)):ms) -> trace ((show (PU.puId pu)) ++ ": receive updateMetaClos") $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          hp = heap pu

          Just (ClosureV closur) = Map.lookup (addr pointer) hp

          closur' = closur { functionName = fnN, Closure.paramCount = count, Closure.paramMissing = missing }

          hp' = Map.insert (addr pointer) (ClosureV closur') hp
          pu' = pu { heap = hp', locked = True }

          pus' = updatePU pus pu'
      _ -> acqua
