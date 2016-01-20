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

data Acqua = Acqua Program Queue [ProcessingUnit] Interconnection FinishFlag (Map.Map String StateValue)
  deriving (Show,Eq)

newAcqua :: Program -> Int -> Acqua
newAcqua p n = Acqua p queue processingUnits newInterconnection False (Map.fromList [("newEnvIds", (NewEnvIds (Map.fromList []))), ("envId",IntVal 0)])
  where
    processingUnits = newProcessingUnits n
    specialProcessingUnit = head processingUnits
    queue = newQueue specialProcessingUnit

untaintAll :: Acqua -> Acqua
untaintAll (Acqua bb q pus i ff s)
  = Acqua bb q pus' i ff s
  where
    pus' = map (\p -> untaint p) pus
