module Simulator.Rules.Messages.EndCopy where

import Data.List
import Logger

import Simulator.Acqua
import Simulator.ProcessingUnit as PU
import Simulator.Interconnection

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

          pu' = pu { enabled = True }

          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgEndCopy (MsgEndCopy pId) 1)]
          (iret, pus') = if (lockedMsg pu)
                         then (i'', pus)
                         else (i', updatePU pus pu')
      _ -> acqua
