module Simulator.Run where

import AcquaIR.Language as IR

import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Queue

type FinishFlag = Bool
data Acqua = Acqua Queue [ProcessingUnit] Interconnection FinishFlag

newAcqua :: Int -> Acqua
newAcqua n = Acqua newQueue (newProcessingUnits n) newInterconnection False

run :: IR.Program -> Int -> String
run program pus_n = step (newAcqua pus_n)

type Rule = Acqua -> Acqua
rules :: [Rule]
rules = []

step :: Acqua -> String
step (Acqua q pus i True) = "Finished!" 
step acqua                = step (applyRules rules acqua)

applyRules :: [Acqua -> Acqua] -> Acqua -> Acqua
applyAll [] a = a
applyAll (f:fs) a = applyAll fs (f a)

