module Simulator.Rules.EndCopy where

import qualified Data.Map as Map
import Data.List
import Logger

import AcquaIR.Language
import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection
import Simulator.Value
import Simulator.CallRecord
import Simulator.CallRecordSeg

import Simulator.Rules.Base

getNextEndCopyMessage :: Interconnection -> Maybe Message
getNextEndCopyMessage [] = Nothing
getNextEndCopyMessage (m:ms) =
   case m of
       ConstMsgEndCopy  _ 0 -> Just m
       _ -> getNextEndCopyMessage ms

endCopy :: Rule
endCopy acqua  =
  let m = getNextEndCopyMessage (interconnection acqua)
  in case m of
      Just (ConstMsgEndCopy (MsgEndCopy pId) 0) -> trace ((show (PU.puId pu)) ++ ": receive endCopy")  $ endCopy $ acqua { processingUnits = pus', interconnection = iret }
        where
          Just m' = m
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          pu' = pu { enabled = True }

          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgEndCopy (MsgEndCopy pId) 1)]
          (iret, pus') = if (lockedMsg pu)
                         then (i'', pus)
                         else (i', updatePU pus pu')
      _ -> acqua
