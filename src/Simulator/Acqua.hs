module Simulator.Acqua where


import AcquaIR.Language as IR
import Simulator.AcquaState
import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Queue

type FinishFlag = Bool

data Acqua = Acqua {
  program :: Program,
  queue :: Queue,
  processingUnits :: [ProcessingUnit],
  interconnection :: Interconnection,
  finishFlag :: FinishFlag,
  acquaState :: AcquaState
} deriving (Show,Eq)

newAcqua :: Program -> Int -> Acqua
newAcqua p n = Acqua p q pus newInterconnection False statesDefault
  where
    specialProcessingUnit = emptySpecialPU
    pus = specialProcessingUnit:(newProcessingUnits n)
    q = newQueue specialProcessingUnit

newAcquaMap :: Program -> Int -> String -> [String] -> Acqua
newAcquaMap p n var params = Acqua p' q pus newInterconnection False statesDefault
  where
    specialProcessingUnit = specialPU (map read params)
    pus = specialProcessingUnit:(newProcessingUnits n)
    q = newQueueForMap specialProcessingUnit (map read params)
    p' = ((addGetVar (head p)):(tail p))
    addGetVar bb = bb { IR.commands = ((GetCallRecordParam "callRecord" 0 var):(IR.commands bb)) }

unlockAll :: Acqua -> Acqua
unlockAll (Acqua bb q pus i ff s)
  = Acqua bb q pus' i ff s
  where
    pus' = map (\p -> unlock p) pus
