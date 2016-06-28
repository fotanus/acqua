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
import Simulator.CallRecord

import Simulator.Rules.Base

receiveUpdateClos :: Rule
receiveUpdateClos acqua =
  case (interconnection acqua) of
      ((ConstMsgUpdateClos (MsgUpdateClos pId pointer idx val)):ms) -> trace ((show (PU.puId pu)) ++ ": receive updateClos, now it is " ++ (show callRec'))  $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          hp = heap pu

          Just (CallRecordV callRec) = Map.lookup (addr pointer) hp

          newParams = Seq.update idx val (params callRec)
          callRec' = callRec { params = newParams }

          hp' = Map.insert (addr pointer) (CallRecordV callRec') hp
          pu' = pu { heap = hp', locked = True }

          pus' = updatePU pus pu'
      _ -> acqua
