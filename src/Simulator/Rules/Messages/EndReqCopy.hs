module Simulator.Rules.Messages.EndReqCopy where

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

getNextEndReqCopyMessage :: Interconnection -> Maybe Message
getNextEndReqCopyMessage [] = Nothing
getNextEndReqCopyMessage (m:ms) =
   case m of
       ConstMsgEndReqCopy  _ 0 -> Just m
       _ -> getNextEndReqCopyMessage ms

endReqCopy :: Rule
endReqCopy acqua  =
  let m = getNextEndReqCopyMessage (interconnection acqua)
  in case m of
      Just (ConstMsgEndReqCopy (MsgEndReqCopy pId) 0) -> trace ((show (PU.puId pu)) ++ ": receive endReqCopy")  $ endReqCopy $ acqua { processingUnits = pus', interconnection = iret }
        where
          Just m' = m
          pus = processingUnits acqua
          Just pu = Data.List.find (\p -> (PU.puId p) == pId) pus
          crseg = callRecordSeg pu

          Just cenv = Map.lookup (currentEnv pu) (environments pu)
          Just (PointerV pointer) = Map.lookup "callRecord" cenv
          Just (CallRecordV callRec) = Map.lookup (addr pointer) crseg
          bb = program acqua
          BB _ _ c t = lookupBB bb (functionName callRec)
          pu' = pu { PU.commands = c, PU.terminator = t, enabled = True }

          i = interconnection acqua
          i' = delete m' i
          i'' = i' ++ [(ConstMsgEndReqCopy (MsgEndReqCopy pId) 1)]
          (iret, pus') = if (lockedMsg pu)
                         then (i'', pus)
                         else (i', updatePU pus pu')
      _ -> acqua
