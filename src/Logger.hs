module Logger where

import Debug.Trace

trace :: String -> a -> a
trace s x = Debug.Trace.trace s x

traceShow :: Show a => a -> b -> b
traceShow x = Debug.Trace.traceShow x
