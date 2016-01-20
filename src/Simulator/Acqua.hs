module Simulator.Acqua where

import qualified Data.Map as Map

import AcquaIR.Language
import Simulator.ProcessingUnit
import Simulator.Interconnection
import Simulator.Queue

type FinishFlag = Bool
data StateValue = IntVal Int
                | NewEnvIds (Map.Map (Int,String) String)
                | RulesUsed (Map.Map String Int)
                deriving (Eq,Show)

type AcquaState = Map.Map String StateValue

statesDefault :: AcquaState
statesDefault = Map.fromList [
    ("newEnvIds", (NewEnvIds (Map.fromList []))),
    ("rulesUsed", (RulesUsed (Map.fromList []))),
    ("envId",IntVal 0)
  ]

countRule :: AcquaState-> String -> AcquaState
countRule s rule = Map.insert rule (RulesUsed newRulesUsed) s
  where
    Just (RulesUsed rulesUsed) = Map.lookup "rulesUsed" s
    Just count = Map.lookup rule rulesUsed
    newRulesUsed = Map.insert rule (count + 1) rulesUsed

data Acqua = Acqua Program Queue [ProcessingUnit] Interconnection FinishFlag AcquaState
  deriving (Show,Eq)

newAcqua :: Program -> Int -> Acqua
newAcqua p n = Acqua p queue processingUnits newInterconnection False statesDefault
  where
    processingUnits = newProcessingUnits n
    specialProcessingUnit = head processingUnits
    queue = newQueue specialProcessingUnit

untaintAll :: Acqua -> Acqua
untaintAll (Acqua bb q pus i ff s)
  = Acqua bb q pus' i ff s
  where
    pus' = map (\p -> untaint p) pus
