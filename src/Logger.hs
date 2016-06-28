-- This module is a wrapper over Debug.Trace that allows it to be disabled

module Logger where

import Debug.Trace as T
import Text.Show.Pretty

import Simulator.Acqua

doTrace :: Bool
doTrace = True

trace :: String -> a -> a
trace msg ret = if doTrace
              then T.trace msg ret
              else ret

traceShow :: Show a => a -> b -> b
traceShow msg ret = if doTrace
                then T.traceShow msg ret
                else ret

traceShowId :: Show a => a -> a
traceShowId ret = if doTrace
                then T.traceShowId ret
                else ret

traceAcqua :: Acqua -> a -> a
traceAcqua acqua ret =
    if doTrace
    then T.traceShow "====Queue===" $
         T.trace (ppShow (queue acqua)) $
         T.traceShow "====Interconnection====" $
         T.trace (ppShow (interconnection acqua)) $
         T.traceShow "====Processing Units====" $
         T.trace (ppShow (processingUnits acqua)) ret
    else ret
