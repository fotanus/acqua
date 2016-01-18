module Simulator.Acqua where

import AcquaIR.Language
import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Queue

type FinishFlag = Bool
data Acqua = Acqua Program Queue [ProcessingUnit] Interconnection FinishFlag

newAcqua :: Program -> Int -> Acqua
newAcqua p n = Acqua p queue processingUnits newInterconnection False
  where 
    processingUnits = newProcessingUnits n
    specialProcessingUnit = head processingUnits
    queue = newQueue specialProcessingUnit
