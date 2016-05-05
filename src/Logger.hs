-- This module is a wrapper over Debug.Trace that allows it to be disabled

module Logger where

import Debug.Trace

doTrace :: Bool
doTrace = True

trace :: String -> a -> a
trace msg ret = if doTrace
              then Debug.Trace.trace msg ret
              else ret

traceShow :: Show a => a -> b -> b
traceShow msg ret = if doTrace
                then Debug.Trace.traceShow msg ret
                else ret

traceShowId :: Show a => a -> a
traceShowId ret = if doTrace
                then Debug.Trace.traceShowId ret
                else ret
