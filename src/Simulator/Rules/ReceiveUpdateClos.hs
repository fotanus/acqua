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
receiveUpdateClos acqua =
  case (interconnection acqua) of
      ((ConstMsgUpdateClos (MsgUpdateClos pId pointer idx val)):ms) -> trace ((show (PU.puId pu)) ++ ": receive updateClos, now it is " ++ (show closur'))  $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          hp = heap pu

          Just (ClosureV closur) = Map.lookup (addr pointer) hp

          newParams = Seq.update idx val (params closur)
          closur' = closur { params = newParams }

          hp' = Map.insert (addr pointer) (ClosureV closur') hp
          pu' = pu { heap = hp', locked = True }

          pus' = updatePU pus pu'
      _ -> acqua
