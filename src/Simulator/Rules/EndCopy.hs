module Simulator.Rules.EndCopy where

import Data.List
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

import Simulator.Rules.Base

endCopy :: Rule
endCopy acqua  =
  let
    i = interconnection acqua
  in case i of
      ((ConstMsgEndCopy (MsgEndCopy pId)):ms) -> trace ((show (PU.puId pu)) ++ ": receive endCopy")  $ acqua { processingUnits = pus', interconnection = ms }
        where
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          pu' = pu { enabled = True }
          pus' = updatePU pus pu'
      _ -> acqua
