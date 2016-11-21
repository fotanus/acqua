module Simulator.Stats where

import qualified Data.Map as Map
import qualified Data.List as List

import AcquaIR.Language
import Simulator.Acqua
import Simulator.AcquaState
import Simulator.Queue
import Simulator.ProcessingUnit as PU

trackStats :: Acqua -> Acqua
trackStats acqua =
  trackSteps $ trackOccupiedPUs $ updateMaxQueueSize $ updateMaxLocalCallRec $ updateMaxCallRec $ updateMaxMsg acqua

trackSteps :: Acqua -> Acqua
trackSteps acqua =
  let
    Just (IntVal steps) = Map.lookup "steps" (acquaState acqua)
    state' = Map.insert "steps" (IntVal (steps+1)) (acquaState acqua)
  in
    acqua { acquaState = state' }


trackOccupiedPUs :: Acqua -> Acqua
trackOccupiedPUs acqua =
  let
    Just (MapIntInt oppc) = Map.lookup "occupiedPUPerCycle" (acquaState acqua)
    oppc' = Map.insert (Map.size oppc) occupiedPUs oppc
    state' = Map.insert "occupiedPUPerCycle" (MapIntInt oppc') (acquaState acqua)
    occupiedPUs = (length (List.filter (\p-> not ((PU.terminator p) == Empty)) (processingUnits acqua)))
  in
    acqua { acquaState = state' }

updateMaxQueueSize :: Acqua -> Acqua
updateMaxQueueSize acqua =
  let
    Just (IntVal currentMax) = Map.lookup "maxQueueSize" (acquaState acqua)
    maxSize = if currentMax > currentQueueSize then currentMax else currentQueueSize
    currentQueueSize = length (jobs (queue acqua))
    state' = Map.insert "maxQueueSize" (IntVal maxSize) (acquaState acqua)
  in
    acqua { acquaState = state' }

updateMaxLocalCallRec :: Acqua -> Acqua
updateMaxLocalCallRec acqua =
  let
    Just (IntVal currentMax) = Map.lookup "maxLocalCallRec" (acquaState acqua)
    maxSize = if currentMax > currentCallRec then currentMax else currentCallRec
    currentCallRec = maximum $ map (\p-> Map.size (environments p)) (processingUnits acqua)
    state' = Map.insert "maxLocalCallRec" (IntVal maxSize) (acquaState acqua)
  in
    acqua { acquaState = state' }

updateMaxCallRec :: Acqua -> Acqua
updateMaxCallRec acqua =
  let
    Just (IntVal currentMax) = Map.lookup "maxCallRec" (acquaState acqua)
    maxSize = if currentMax > currentCallRec then currentMax else currentCallRec
    currentCallRec = maximum $ map (\p-> occupiedMemory p) (processingUnits acqua)
    state' = Map.insert "maxCallRec" (IntVal maxSize) (acquaState acqua)
  in
    acqua { acquaState = state' }

updateMaxMsg :: Acqua -> Acqua
updateMaxMsg acqua =
  let
    Just (IntVal currentMax) = Map.lookup "maxMsg" (acquaState acqua)
    maxSize = if currentMax > currentMsgs then currentMax else currentMsgs
    currentMsgs = length (interconnection acqua)
    state' = Map.insert "maxMsg" (IntVal maxSize) (acquaState acqua)
  in
    acqua { acquaState = state' }
