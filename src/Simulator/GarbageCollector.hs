module Simulator.GarbageCollector where

import qualified Data.Map as Map

import Simulator.Acqua
import Simulator.ProcessingUnit
import Simulator.CallRecord
import Simulator.CallRecordSeg

garbageCollector :: Acqua -> Acqua
garbageCollector acqua =
    acqua { processingUnits = pus' }
  where
    pus' = map collect $ processingUnits acqua
    collect pu = pu { callRecordSeg = crseg' }
      where
        crseg' = Map.fromList $ filter deleteIfZero $ map decCounters $ Map.toList $ callRecordSeg pu

    deleteIfZero (_,v) = case v of
                          CallRecordV cr -> timeout cr > 0
                          _ -> True

    decCounters (k,v) = (k, v')
      where
        v' = case v of
              CallRecordV cr -> if isMap cr && timeout cr <= maxTimeout
                                then CallRecordV $ cr { timeout = (timeout cr) - 1 }
                                else CallRecordV $ cr
              _ -> v


