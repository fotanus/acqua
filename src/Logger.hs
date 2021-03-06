-- This module is a wrapper over Debug.Trace that allows it to be disabled

module Logger where

import Debug.Trace as T
import Text.Show.Pretty as P

import Simulator.Acqua


ppShow :: Show a => a -> String
ppShow = P.ppShow

doTrace :: Bool
doTrace = True

trace :: String -> a -> a
trace msg ret = if doTrace
              then T.trace msg ret
              else ret

traceShow :: Show a => a -> b -> b
traceShow msg ret = if doTrace
                then T.trace (P.ppShow msg) ret
                else ret

traceShowId :: Show a => a -> a
traceShowId ret = if doTrace
                then T.trace (P.ppShow ret) ret
                else ret

traceAcqua :: Acqua -> a -> a
traceAcqua acqua ret =
    if doTrace
    then T.traceShow "====Queue===" $
         T.trace (P.ppShow (queue acqua)) $
         T.traceShow "====Interconnection====" $
         T.trace (P.ppShow (interconnection acqua)) $
         T.traceShow "====Processing Units====" $
         T.trace (P.ppShow (processingUnits acqua)) ret
    else ret
