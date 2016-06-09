module Simulator.Acqua where

import qualified Data.Map as Map

import AcquaIR.Language
import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Queue

type FinishFlag = Bool
data StateValue = IntVal Int
                | NewEnvIds (Map.Map (Int,String) String)
                deriving (Eq,Show)

type AcquaState = Map.Map String StateValue

data Acqua = Acqua Program Queue [ProcessingUnit] Interconnection FinishFlag AcquaState
  deriving (Show,Eq)

statesDefault :: AcquaState
statesDefault = Map.fromList [
    ("newEnvIds", (NewEnvIds (Map.fromList []))),
    ("envId",IntVal 0)
  ]

newAcqua :: Program -> Int -> Acqua
newAcqua p n = Acqua p queue processingUnits newInterconnection False statesDefault
  where
    processingUnits = newProcessingUnits n
    specialProcessingUnit = head processingUnits
    queue = newQueue specialProcessingUnit

unlockAll :: Acqua -> Acqua
unlockAll (Acqua bb q pus i ff s)
  = Acqua bb q pus' i ff s
  where
    pus' = map (\p -> unlock p) pus
